import antlr4 from 'antlr4';
import CSharpLexer from './CSharpLexer.js';
import CSharpParser from './CSharpParser.js';

export default class CSharpParserBase extends antlr4.Parser {
    constructor(input) {
        super(input);
        this._symTable = null;
        this._pendingVarType = '?';
    }

    // -------------------------------------------------------------------------
    // Symbol table — lazily initialised
    // -------------------------------------------------------------------------

    get symTable() {
        if (!this._symTable) {
            this._symTable = new CSharpSymbolTable();
        }
        return this._symTable;
    }

    // -------------------------------------------------------------------------
    // Look-ahead helpers
    // -------------------------------------------------------------------------

    LookAheadIs(pos, value) {
        return this._input.LA(pos) === value;
    }

    LookAheadIsNot(pos, value) {
        return this._input.LA(pos) !== value;
    }

    // -------------------------------------------------------------------------
    // Error reporting
    // -------------------------------------------------------------------------

    _notifySemanticError(line, charPositionInLine, msg) {
        this.notifyErrorListeners(msg);
    }

    // -------------------------------------------------------------------------
    // ReduceTree — post-parse cleanup (for external tooling)
    // -------------------------------------------------------------------------

    ReduceTree(currentctx) {
        const ruleNames = this.ruleNames;
        const ruleTypeArgumentList = ruleNames.indexOf("type_argument_list");

        const takeOutEmpties = (node) => {
            if (!node || !node.children) return;
            for (let ix = node.children.length - 1; ix >= 0; ix--) {
                const child = node.children[ix];
                if (child && child.ruleIndex !== undefined) {
                    if (child.ruleIndex === ruleTypeArgumentList && child.getChildCount() === 0)
                        node.children.splice(ix, 1);
                    else
                        takeOutEmpties(child);
                }
            }
        };
        takeOutEmpties(currentctx);

        const reduceTree = process.env.ANTLR_REDUCE_TREE === "yes";
        if (!reduceTree) return;

        const reduceAllChildren = process.env.ANTLR_REDUCE_ALL_CHILDREN === "yes";

        const reducer = (node) => {
            if (!node || !node.children) return;
            for (const child of node.children) reducer(child);
            if (reduceAllChildren || node.getChildCount() === 1) {
                for (let i = 0; i < node.children.length; i++) {
                    const child = node.children[i];
                    if (child && child.ruleIndex !== undefined && child.getChildCount() === 1) {
                        const grandchild = child.getChild(0);
                        if (grandchild && grandchild.ruleIndex !== undefined)
                            node.children[i] = grandchild;
                    }
                }
            }
        };
        if (currentctx.children) reducer(currentctx);
    }

    // -------------------------------------------------------------------------
    // _insertNode — rewire parse tree (MLR un-inlining)
    // -------------------------------------------------------------------------

    _insertNode(currentctx, contextTypeName) {
    return;
        const CtxCls = this.constructor[contextTypeName];
        if (!CtxCls) return;
        const inserted = new CtxCls(currentctx, currentctx.invokingState);
        inserted.children = currentctx.children ? [...currentctx.children] : [];
        currentctx.children = [inserted];
    }

    AsInvocationExpression(currentctx)         { this._insertNode(currentctx, "Invocation_expressionContext"); }
    AsElementAccess(currentctx)                { this._insertNode(currentctx, "Element_accessContext"); }
    AsMemberAccess(currentctx)                 { this._insertNode(currentctx, "Member_accessContext"); }
    AsNullConditionalMemberAccess(currentctx)  { this._insertNode(currentctx, "Null_conditional_member_accessContext"); }
    AsNullConditionalElementAccess(currentctx) { this._insertNode(currentctx, "Null_conditional_element_accessContext"); }
    AsPostIncrementExpression(currentctx)      { this._insertNode(currentctx, "Post_increment_expressionContext"); }
    AsPostDecrementExpression(currentctx)      { this._insertNode(currentctx, "Post_decrement_expressionContext"); }
    AsNullForgivingExpression(currentctx)      { this._insertNode(currentctx, "Null_forgiving_expressionContext"); }
    AsPointerMemberAccess(currentctx)          { this._insertNode(currentctx, "Pointer_member_accessContext"); }

    // -------------------------------------------------------------------------
    // ElementAccessSemanticCheck
    // -------------------------------------------------------------------------

    ElementAccessSemanticCheck(currentctx) {
        const ruleNames = this.ruleNames;
        const rIdx = (name) => ruleNames.indexOf(name);

        const rulePrimaryExpression            = rIdx("primary_expression");
        const ruleElementAccess                = rIdx("element_access");
        const rulePointerElementAccess         = rIdx("pointer_element_access");
        const ruleNullConditionalElementAccess = rIdx("null_conditional_element_access");
        const ruleArrayCreationExpression      = rIdx("array_creation_expression");
        const ruleStackallocExpression         = rIdx("stackalloc_expression");

        if (!currentctx || currentctx.ruleIndex !== rulePrimaryExpression
                || currentctx.getChildCount() !== 1) return;

        const childRule = currentctx.getChild(0);
        if (!childRule || childRule.ruleIndex === undefined) return;

        const childRuleIndex  = childRule.ruleIndex;
        const childChildCount = childRule.getChildCount();

        if (childRuleIndex === ruleElementAccess || childRuleIndex === rulePointerElementAccess) {
            if (childChildCount !== 4) return;
        } else if (childRuleIndex === ruleNullConditionalElementAccess) {
            if (childChildCount !== 5) return;
        } else return;

        const accessTarget = childRule.getChild(0);
        if (!accessTarget || accessTarget.ruleIndex === undefined) return;
        if (accessTarget.ruleIndex !== rulePrimaryExpression || accessTarget.getChildCount() === 0) return;

        const lhsTarget = accessTarget.getChild(0);
        if (!lhsTarget || lhsTarget.ruleIndex === undefined) return;

        if (lhsTarget.ruleIndex !== ruleArrayCreationExpression
                && lhsTarget.ruleIndex !== ruleStackallocExpression) return;

        const lhsLast     = lhsTarget.stop;
        const lhsLastType = lhsLast ? lhsLast.type : -1;

        if (lhsLastType === CSharpLexer.TK_RBRACE) return;

        if (lhsLastType !== CSharpLexer.TK_RBRACK) {
            process.stderr.write(
                `${lhsLast.line}:${lhsLast.column} Error: Unexpected LHS last token ${lhsLast.text} (${lhsLastType}).\n`);
            return;
        }

        const childRuleName = ruleNames[childRuleIndex];
        const lhsRuleName   = ruleNames[lhsTarget.ruleIndex];
        const childPrefix   = "AEIOUaeiou".includes(childRuleName[0]) ? "an" : "a";
        const lhsPrefix     = "AEIOUaeiou".includes(lhsRuleName[0])   ? "an" : "a";

        this._notifySemanticError(lhsLast.line, lhsLast.column,
            `LHS of ${childPrefix} ${childRuleName} cannot be ${lhsPrefix} ${lhsRuleName} unless it has an initializer`);
    }

    // =========================================================================
    // Scope management
    // =========================================================================

    EnterNamespaceScope() { this.symTable.enterScope(CSharpScopeKind.Namespace); }
    EnterTypeScope()      { this.symTable.enterScope(CSharpScopeKind.Type); }
    EnterBlockScope()     { this.symTable.enterScope(CSharpScopeKind.Block); }
    ExitCurrentScope()    { this.symTable.exitScope(); }

    // =========================================================================
    // Grammar actions — using directive, type parameter, variable declarator
    // =========================================================================

    OnTypeParameter() {
        const ctx = this._ctx;
        if (!ctx) return;
        const name = ctx.getChild(ctx.getChildCount() - 1).getText();
        this.symTable.declareTypeParam(name);
    }

    OnUsingAliasDirective() {
        const ctx = this._ctx;
        if (!ctx) return;
        this.symTable.declareAlias(ctx.getChild(1).getText(), ctx.getChild(3).getText());
    }

    OnUsingNamespaceDirective() {
        const ctx = this._ctx;
        if (!ctx) return;
        this.symTable.importNamespace(ctx.getChild(1).getText());
    }

    BeginVariableDeclaration() {
        const ctx = this._ctx;
        if (!ctx) return;
        this._pendingVarType = ctx.getChild(ctx.getChildCount() - 1).getText() || '?';
    }

    OnVariableDeclarator() {
        const ctx = this._ctx;
        if (!ctx) return;
        const id = ctx.getChild(ctx.getChildCount() - 1).getText();
        this.symTable.declareVariable(id, this._pendingVarType);
    }

    // =========================================================================
    // Semantic predicates
    // =========================================================================

    IsCastExpressionAhead() {
        const tok = this._input.LT(2);
        if (!tok) return true;
        const sym = this.symTable.currentScope.lookupChain(tok.text);
        return sym === null || sym.kind !== CSharpSymbolKind.Variable;
    }

    IsTypeParameterName() {
        const tok = this._input.LT(1);
        if (!tok) return false;
        const sym = this.symTable.currentScope.lookupChain(tok.text);
        return sym !== null && sym.kind === CSharpSymbolKind.TypeParameter;
    }

    IsValueTypeName() {
        const tok = this._input.LT(1);
        if (!tok) return false;
        const vt = new Set([
            CSharpLexer.KW_BOOL,  CSharpLexer.KW_BYTE,   CSharpLexer.KW_CHAR,
            CSharpLexer.KW_DECIMAL, CSharpLexer.KW_DOUBLE, CSharpLexer.KW_FLOAT,
            CSharpLexer.KW_INT,   CSharpLexer.KW_LONG,   CSharpLexer.KW_SBYTE,
            CSharpLexer.KW_SHORT, CSharpLexer.KW_UINT,   CSharpLexer.KW_ULONG,
            CSharpLexer.KW_USHORT, CSharpLexer.TK_LPAREN,
        ]);
        if (vt.has(tok.type)) return true;
        const sym = this.symTable.currentScope.lookupChain(tok.text);
        if (!sym || sym.kind !== CSharpSymbolKind.Type) return false;
        return sym.typeKind === CSharpTypeKind.Struct || sym.typeKind === CSharpTypeKind.Enum;
    }

    IsReferenceTypeName() {
        const tok = this._input.LT(1);
        if (!tok) return true;
        if (tok.type === CSharpLexer.KW_DYNAMIC || tok.type === CSharpLexer.KW_OBJECT ||
            tok.type === CSharpLexer.KW_STRING  || tok.type === CSharpLexer.TK_LBRACK) return true;
        const sym = this.symTable.currentScope.lookupChain(tok.text);
        if (!sym) return true;
        if (sym.kind === CSharpSymbolKind.TypeParameter) return false;
        if (sym.kind === CSharpSymbolKind.Type &&
            (sym.typeKind === CSharpTypeKind.Struct || sym.typeKind === CSharpTypeKind.Enum)) return false;
        return true;
    }

    IsDelegateTypeName() {
        const tok = this._input.LT(1);
        if (!tok) return false;
        const sym = this.symTable.currentScope.lookupChain(tok.text);
        return sym !== null && sym.kind === CSharpSymbolKind.Type && sym.typeKind === CSharpTypeKind.Delegate;
    }

    IsInterfaceTypeName() {
        const tok = this._input.LT(1);
        if (!tok) return false;
        const sym = this.symTable.currentScope.lookupChain(tok.text);
        return sym !== null && sym.kind === CSharpSymbolKind.Type && sym.typeKind === CSharpTypeKind.Interface;
    }

    IsClassTypeName() {
        const tok = this._input.LT(1);
        if (!tok) return true;
        if (tok.type === CSharpLexer.KW_OBJECT || tok.type === CSharpLexer.KW_STRING) return true;
        const sym = this.symTable.currentScope.lookupChain(tok.text);
        if (!sym) return true;
        if (sym.kind !== CSharpSymbolKind.Type) return true;
        return sym.typeKind !== CSharpTypeKind.Interface && sym.typeKind !== CSharpTypeKind.Delegate;
    }

    _classBaseTypeCheck(wantInterface) {
        const tok = this._input.LT(2);
        if (!tok) return !wantInterface;
        if (tok.type === CSharpLexer.KW_OBJECT || tok.type === CSharpLexer.KW_STRING) return !wantInterface;
        const sym = this.symTable.currentScope.lookupChain(tok.text);
        if (!sym || sym.kind !== CSharpSymbolKind.Type) return !wantInterface;
        const isIface = sym.typeKind === CSharpTypeKind.Interface;
        return wantInterface ? isIface : !isIface;
    }

    IsClassBaseInterfaceList() { return this._classBaseTypeCheck(true); }
    IsClassBaseClassType()     { return this._classBaseTypeCheck(false); }

    IsDeclarationPatternAhead() {
        // Heuristic lookahead (avoids stream corruption from Parser constructor
        // reset() and BailErrorStrategy.sync() silently consuming mismatched tokens):
        // A declaration_pattern is "type simple_designation".
        // If LT(1) is a value-type keyword, check LT(2) is a designator.
        // If LT(1) is an identifier known to be a type, check LT(2) likewise.
        // Otherwise (string/number literals, unknown names) it is a constant_pattern.
        const tok1 = this._input.LT(1);
        if (!tok1) return false;
        const vt = new Set([CSharpLexer.KW_BOOL, CSharpLexer.KW_BYTE, CSharpLexer.KW_CHAR,
            CSharpLexer.KW_DECIMAL, CSharpLexer.KW_DOUBLE, CSharpLexer.KW_FLOAT,
            CSharpLexer.KW_INT, CSharpLexer.KW_LONG, CSharpLexer.KW_SBYTE,
            CSharpLexer.KW_SHORT, CSharpLexer.KW_UINT, CSharpLexer.KW_ULONG,
            CSharpLexer.KW_USHORT]);
        if (vt.has(tok1.type)) {
            const tok2 = this._input.LT(2);
            return tok2 !== null && (tok2.type === CSharpLexer.Simple_Identifier || tok2.text === '_');
        }
        if (tok1.type !== CSharpLexer.Simple_Identifier) return false;
        const sym = this.symTable.currentScope.lookupChain(tok1.text);
        const isKnownType = sym !== null && sym.kind === CSharpSymbolKind.Type;
        if (!isKnownType) return false;
        const tok2 = this._input.LT(2);
        return tok2 !== null && (tok2.type === CSharpLexer.Simple_Identifier || tok2.text === '_');
    }

    IsConstantPatternAhead() { return !this.IsDeclarationPatternAhead(); }

    IsImplicitlyTypedLocalVariable() {
        const tok = this._input.LT(1);
        if (!tok) return true;
        if (tok.type !== CSharpLexer.KW_VAR) return false;
        const sym = this.symTable.currentScope.lookupChain('var');
        if (sym && sym.kind === CSharpSymbolKind.Type) return false;
        const lt3 = this._input.LT(3);
        if (!lt3 || lt3.text !== '=') return false;
        const lt4 = this._input.LT(4);
        if (lt4 && lt4.text === '{') return false;
        return true;
    }

    IsExplicitlyTypedLocalVariable() {
        const tok = this._input.LT(1);
        if (!tok) return true;
        if (tok.type !== CSharpLexer.KW_VAR) return true;
        const sym = this.symTable.currentScope.lookupChain('var');
        if (sym && sym.kind === CSharpSymbolKind.Type) return true;
        const lt3 = this._input.LT(3);
        if (!lt3 || lt3.text !== '=') return true;
        const lt4 = this._input.LT(4);
        if (lt4 && lt4.text === '{') return true;
        return false;
    }

    IsExplicitlyTypedRefLocalVariable() {
        const tok = this._input.LT(1);
        return tok !== null && tok.type === CSharpLexer.KW_REF;
    }

    // Gates comma-separated declarators: false when type is 'var' (only one declarator allowed).
    IsLocalVariableDeclaration() {
        const local_var_decl = this._ctx;
        if (!(local_var_decl instanceof CSharpParser.Local_variable_declarationContext)) return true;
        const local_variable_type = local_var_decl.local_variable_type();
        if (local_variable_type == null) return true;
        return local_variable_type.getText() !== "var";
    }
}

// =============================================================================
// Symbol table helpers
// =============================================================================

const CSharpSymbolKind = Object.freeze({
    Type: 'Type', Variable: 'Variable', Namespace: 'Namespace',
    TypeParameter: 'TypeParameter', Alias: 'Alias',
});
const CSharpTypeKind = Object.freeze({
    Class: 'Class', Struct: 'Struct', Interface: 'Interface',
    Enum: 'Enum', Delegate: 'Delegate',
});
const CSharpScopeKind = Object.freeze({
    Global: 'Global', Namespace: 'Namespace', Type: 'Type',
    Method: 'Method', Block: 'Block',
});

class CSharpScope {
    constructor(kind, parent, name = '') {
        this.kind = kind; this.parent = parent; this.name = name;
        this._symbols = new Map();
    }
    declare(sym) { this._symbols.set(sym.name, sym); }
    lookup(name) { return this._symbols.get(name) ?? null; }
    lookupChain(name) {
        const s = this.lookup(name);
        if (s !== null) return s;
        return this.parent ? this.parent.lookupChain(name) : null;
    }
}

const _CS_RESERVED = new Set([
    'abstract','as','base','bool','break','byte','case','catch','char','checked',
    'class','const','continue','decimal','default','delegate','do','double','else',
    'enum','event','explicit','extern','false','finally','fixed','float','for',
    'foreach','goto','if','implicit','in','int','interface','internal','is','lock',
    'long','namespace','new','null','object','operator','out','override','params',
    'private','protected','public','readonly','ref','return','sbyte','sealed','short',
    'sizeof','stackalloc','static','string','struct','switch','this','throw','true',
    'try','typeof','uint','ulong','unchecked','unsafe','ushort','using','virtual',
    'void','volatile','while',
]);

function _csIsIdentLike(t) { return t ? /^[A-Za-z_@]/.test(t) : false; }

class CSharpSymbolTable {
    constructor() {
        this._knownTypeNames = new Set();
        this._genericArities = new Map();
        this._scopeStack = [];
        this.globalScope = new CSharpScope(CSharpScopeKind.Global, null, '<global>');
        this._scopeStack.push(this.globalScope);
        this._populateBuiltins();
    }

    get currentScope() { return this._scopeStack[this._scopeStack.length - 1]; }

    isTypeName(name) {
        const sym = this.currentScope.lookupChain(name);
        if (sym !== null)
            return sym.kind === CSharpSymbolKind.Type ||
                   sym.kind === CSharpSymbolKind.TypeParameter ||
                   sym.kind === CSharpSymbolKind.Alias;
        return this._knownTypeNames.has(name);
    }

    enterScope(kind, name = '') {
        this._scopeStack.push(new CSharpScope(kind, this.currentScope, name));
    }

    exitScope() { if (this._scopeStack.length > 1) this._scopeStack.pop(); }

    declareType(name, kind, arity = 0) {
        this.currentScope.declare({ name, kind: CSharpSymbolKind.Type, typeKind: kind, arity });
        this._knownTypeNames.add(name);
        if (arity > 0) {
            if (!this._genericArities.has(name)) this._genericArities.set(name, new Set());
            this._genericArities.get(name).add(arity);
        }
    }

    declareTypeParam(name) {
        this.currentScope.declare({ name, kind: CSharpSymbolKind.TypeParameter });
        this._knownTypeNames.add(name);
    }

    declareVariable(name, typeRef) {
        this.currentScope.declare({ name, kind: CSharpSymbolKind.Variable, typeRef });
    }

    declareAlias(alias, target) {
        this.currentScope.declare({ name: alias, kind: CSharpSymbolKind.Alias, target });
        this._knownTypeNames.add(alias);
    }

    importNamespace(ns) {
        this.currentScope.declare({ name: ns, kind: CSharpSymbolKind.Namespace });
    }

    preScan(tokens) {
        try { tokens.fill(); } catch (e) {}
        const n = tokens.size;
        for (let i = 0; i < n; i++) {
            const tok = tokens.get(i);
            if (tok.channel !== 0) continue;
            const txt = tok.text;
            if (txt === 'class' || txt === 'struct' || txt === 'interface' || txt === 'enum') {
                let j = this._nextDefault(tokens, i + 1, n);
                while (j < n && (tokens.get(j).text === 'partial' || tokens.get(j).text === 'ref'))
                    j = this._nextDefault(tokens, j + 1, n);
                if (j < n && _csIsIdentLike(tokens.get(j).text)) {
                    const typeName = tokens.get(j).text;
                    const [arity, typeParams] = this._countTypeParams(tokens, j + 1, n);
                    this._registerType(typeName, arity, typeParams);
                }
            } else if (txt === 'delegate') {
                let j = this._nextDefault(tokens, i + 1, n);
                while (j < n && (tokens.get(j).text === 'ref' || tokens.get(j).text === 'readonly'))
                    j = this._nextDefault(tokens, j + 1, n);
                if (j >= n) continue;
                j = this._nextDefault(tokens, j + 1, n);
                if (j < n && tokens.get(j).text === '<') j = this._skipAngled(tokens, j, n);
                while (j < n && (tokens.get(j).text === '[' || tokens.get(j).text === ']' || tokens.get(j).text === ','))
                    j = this._nextDefault(tokens, j + 1, n);
                if (j < n && tokens.get(j).text === '?') j = this._nextDefault(tokens, j + 1, n);
                if (j < n && _csIsIdentLike(tokens.get(j).text)) {
                    const typeName = tokens.get(j).text;
                    const [arity, typeParams] = this._countTypeParams(tokens, j + 1, n);
                    this._registerType(typeName, arity, typeParams);
                }
            }
        }
    }

    _registerType(name, arity, typeParams) {
        if (!_CS_RESERVED.has(name)) this._knownTypeNames.add(name);
        if (arity > 0 && !_CS_RESERVED.has(name)) {
            if (!this._genericArities.has(name)) this._genericArities.set(name, new Set());
            this._genericArities.get(name).add(arity);
        }
        for (const tp of typeParams)
            if (_csIsIdentLike(tp) && !_CS_RESERVED.has(tp)) this._knownTypeNames.add(tp);
    }

    _nextDefault(tokens, pos, n) {
        while (pos < n && tokens.get(pos).channel !== 0) pos++;
        return pos;
    }

    _skipAngled(tokens, pos, n) {
        let depth = 1, i = this._nextDefault(tokens, pos + 1, n);
        while (i < n && depth > 0) {
            const t = tokens.get(i).text;
            if (t === '<') depth++; else if (t === '>') depth--;
            i = this._nextDefault(tokens, i + 1, n);
        }
        return i;
    }

    _countTypeParams(tokens, pos, n) {
        const p = this._nextDefault(tokens, pos, n);
        if (p >= n || tokens.get(p).text !== '<') return [0, []];
        let depth = 1, arity = 1;
        const names = [];
        let i = this._nextDefault(tokens, p + 1, n);
        while (i < n && depth > 0) {
            const t = tokens.get(i).text;
            if (t === '<') depth++;
            else if (t === '>') depth--;
            else if (t === ',' && depth === 1) arity++;
            else if (depth === 1 && _csIsIdentLike(t)) names.push(t);
            i = this._nextDefault(tokens, i + 1, n);
        }
        return depth === 0 ? [arity, names] : [0, []];
    }

    _populateBuiltins() {
        for (const t of [
            'bool','byte','sbyte','char','decimal','double','float','int','uint',
            'long','ulong','short','ushort','object','string','void','dynamic',
            'Boolean','Byte','SByte','Char','Decimal','Double','Single',
            'Int16','Int32','Int64','UInt16','UInt32','UInt64',
            'IntPtr','UIntPtr','Object','String','Void',
            'Guid','DateTime','DateTimeOffset','TimeSpan','Uri',
            'Exception','Type','Enum','Delegate','MulticastDelegate',
            'Attribute','Math','Convert','Console','Environment',
            'List','IList','IEnumerable','IEnumerator','ICollection',
            'IReadOnlyList','IReadOnlyCollection','IReadOnlyDictionary',
            'Dictionary','IDictionary','SortedDictionary','SortedList',
            'HashSet','SortedSet','Queue','Stack','LinkedList','LinkedListNode',
            'Func','Action','Predicate','Comparison','Converter',
            'Task','ValueTask','Nullable',
            'IAsyncEnumerable','IAsyncEnumerator',
            'Span','ReadOnlySpan','Memory','ReadOnlyMemory',
            'KeyValuePair','Tuple',
            'ImmutableArray','ImmutableList','ImmutableDictionary','ImmutableHashSet',
            'Lazy','WeakReference','EventHandler',
            'IEqualityComparer','IComparer','EqualityComparer','Comparer',
            'ConcurrentDictionary','ConcurrentQueue','ConcurrentStack','ConcurrentBag',
        ]) this._knownTypeNames.add(t);
    }
}
