import { Parser, ParserRuleContext, TerminalNode, TokenStream, BailErrorStrategy } from "antlr4ng";
import { CSharpLexer } from "./CSharpLexer.js";
import { Local_variable_declarationContext } from "./CSharpParser.js";

export abstract class CSharpParserBase extends Parser {

    constructor(input: TokenStream) {
        super(input);
    }

    // -------------------------------------------------------------------------
    // Look-ahead helpers
    // -------------------------------------------------------------------------

    LookAheadIs(pos: number, value: number): boolean {
        return this.tokenStream.LA(pos) === value;
    }

    LookAheadIsNot(pos: number, value: number): boolean {
        return this.tokenStream.LA(pos) !== value;
    }

    // -------------------------------------------------------------------------
    // Error reporting
    // -------------------------------------------------------------------------

    private _notifySemanticError(line: number, charPositionInLine: number, msg: string): void {
        this.notifyErrorListeners(msg, null, null);
    }

    // -------------------------------------------------------------------------
    // ReduceTree — post-parse cleanup (for external tooling)
    // -------------------------------------------------------------------------

    ReduceTree(currentctx: ParserRuleContext): void {
        const ruleTypeArgumentList = this.ruleNames.indexOf("type_argument_list");

        const takeOutEmpties = (node: ParserRuleContext): void => {
            if (!node.children) return;
            for (let ix = node.children.length - 1; ix >= 0; ix--) {
                const child = node.children[ix];
                if (child instanceof ParserRuleContext) {
                    if (child.ruleIndex === ruleTypeArgumentList && child.getChildCount() === 0)
                        node.children.splice(ix, 1);
                    else
                        takeOutEmpties(child);
                }
            }
        };
        takeOutEmpties(currentctx);

        const reduceTree = process.env["ANTLR_REDUCE_TREE"] === "yes";
        if (!reduceTree) return;

        const reduceAllChildren = process.env["ANTLR_REDUCE_ALL_CHILDREN"] === "yes";

        const reducer = (node: ParserRuleContext): void => {
            if (!node.children) return;
            for (const child of node.children)
                if (child instanceof ParserRuleContext) reducer(child);
            if (reduceAllChildren || node.getChildCount() === 1) {
                for (let i = 0; i < node.children.length; i++) {
                    const child = node.children[i];
                    if (child instanceof ParserRuleContext && child.getChildCount() === 1) {
                        const grandchild = child.getChild(0);
                        if (grandchild instanceof ParserRuleContext)
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

    private _insertNode(currentctx: ParserRuleContext, contextTypeName: string): void {
        return;
        // const CtxCls = (this.constructor as any)[contextTypeName] as (new (parent: ParserRuleContext, invokingState: number) => ParserRuleContext) | undefined;
        // if (!CtxCls) return;
        // const inserted = new CtxCls(currentctx, currentctx.invokingState);
        // (inserted as any).children = currentctx.children ? [...currentctx.children] : [];
        // (currentctx as any).children = [inserted];
    }

    AsInvocationExpression(currentctx: ParserRuleContext): void         { this._insertNode(currentctx, "Invocation_expressionContext"); }
    AsElementAccess(currentctx: ParserRuleContext): void                { this._insertNode(currentctx, "Element_accessContext"); }
    AsMemberAccess(currentctx: ParserRuleContext): void                 { this._insertNode(currentctx, "Member_accessContext"); }
    AsNullConditionalMemberAccess(currentctx: ParserRuleContext): void  { this._insertNode(currentctx, "Null_conditional_member_accessContext"); }
    AsNullConditionalElementAccess(currentctx: ParserRuleContext): void { this._insertNode(currentctx, "Null_conditional_element_accessContext"); }
    AsPostIncrementExpression(currentctx: ParserRuleContext): void      { this._insertNode(currentctx, "Post_increment_expressionContext"); }
    AsPostDecrementExpression(currentctx: ParserRuleContext): void      { this._insertNode(currentctx, "Post_decrement_expressionContext"); }
    AsNullForgivingExpression(currentctx: ParserRuleContext): void      { this._insertNode(currentctx, "Null_forgiving_expressionContext"); }
    AsPointerMemberAccess(currentctx: ParserRuleContext): void          { this._insertNode(currentctx, "Pointer_member_accessContext"); }

    // -------------------------------------------------------------------------
    // ElementAccessSemanticCheck
    // -------------------------------------------------------------------------

    ElementAccessSemanticCheck(currentctx: ParserRuleContext): void {
        const rIdx = (name: string): number => this.ruleNames.indexOf(name);

        const rulePrimaryExpression            = rIdx("primary_expression");
        const ruleElementAccess                = rIdx("element_access");
        const rulePointerElementAccess         = rIdx("pointer_element_access");
        const ruleNullConditionalElementAccess = rIdx("null_conditional_element_access");
        const ruleArrayCreationExpression      = rIdx("array_creation_expression");
        const ruleStackallocExpression         = rIdx("stackalloc_expression");

        if (currentctx.ruleIndex !== rulePrimaryExpression || currentctx.getChildCount() !== 1) return;

        const childRule = currentctx.getChild(0);
        if (!(childRule instanceof ParserRuleContext)) return;

        const childRuleIndex  = childRule.ruleIndex;
        const childChildCount = childRule.getChildCount();

        if (childRuleIndex === ruleElementAccess || childRuleIndex === rulePointerElementAccess) {
            if (childChildCount !== 4) return;
        } else if (childRuleIndex === ruleNullConditionalElementAccess) {
            if (childChildCount !== 5) return;
        } else return;

        const accessTargetTree = childRule.getChild(0);
        if (!(accessTargetTree instanceof ParserRuleContext)) return;
        if (accessTargetTree.ruleIndex !== rulePrimaryExpression || accessTargetTree.getChildCount() === 0) return;

        const lhsTargetTree = accessTargetTree.getChild(0);
        if (!(lhsTargetTree instanceof ParserRuleContext)) return;

        if (lhsTargetTree.ruleIndex !== ruleArrayCreationExpression
                && lhsTargetTree.ruleIndex !== ruleStackallocExpression) return;

        const lhsLast     = lhsTargetTree.stop;
        const lhsLastType = lhsLast ? lhsLast.type : -1;

        if (lhsLastType === CSharpLexer.TK_RBRACE) return; // initializer present

        if (lhsLastType !== CSharpLexer.TK_RBRACK) {
            process.stderr.write(
                `${lhsLast?.line}:${lhsLast?.column} Error: Unexpected LHS last token ${lhsLast?.text} (${lhsLastType}).\n`);
            return;
        }

        const childRuleName = this.ruleNames[childRuleIndex];
        const lhsRuleName   = this.ruleNames[lhsTargetTree.ruleIndex];
        const childPrefix   = "AEIOUaeiou".includes(childRuleName[0]) ? "an" : "a";
        const lhsPrefix     = "AEIOUaeiou".includes(lhsRuleName[0])   ? "an" : "a";

        this._notifySemanticError(lhsLast?.line ?? 0, lhsLast?.column ?? 0,
            `LHS of ${childPrefix} ${childRuleName} cannot be ${lhsPrefix} ${lhsRuleName} unless it has an initializer`);
    }

    // =========================================================================
    // Symbol table
    // =========================================================================

    private _symTable: Ng_CSharpSymbolTable | undefined;
    private _pendingVarType: string = "?";

    get symTable(): Ng_CSharpSymbolTable {
        if (!this._symTable) { this._symTable = new Ng_CSharpSymbolTable(); this._pendingVarType = "?"; }
        return this._symTable;
    }

    EnterNamespaceScope(): void { this.symTable.enterScope(Ng_CSharpScopeKind.Namespace); }
    EnterTypeScope():      void { this.symTable.enterScope(Ng_CSharpScopeKind.Type); }
    EnterBlockScope():     void { this.symTable.enterScope(Ng_CSharpScopeKind.Block); }
    ExitCurrentScope():    void { this.symTable.exitScope(); }

    OnTypeParameter(): void {
        const ctx = this.context as any;
        const name: string = ctx.getChild(ctx.getChildCount() - 1).getText();
        this.symTable.declareTypeParam(name);
    }
    OnUsingAliasDirective(): void {
        const ctx = this.context as any;
        this.symTable.declareAlias(ctx.getChild(1).getText(), ctx.getChild(3).getText());
    }
    OnUsingNamespaceDirective(): void {
        const ctx = this.context as any;
        this.symTable.importNamespace(ctx.getChild(1).getText());
    }
    BeginVariableDeclaration(): void {
        const ctx = this.context as any;
        this._pendingVarType = ctx.getChild(ctx.getChildCount() - 1).getText() ?? "?";
    }
    OnVariableDeclarator(): void {
        const ctx = this.context as any;
        const id: string = ctx.getChild(ctx.getChildCount() - 1).getText();
        this.symTable.declareVariable(id, this._pendingVarType);
    }

    IsCastExpressionAhead(): boolean {
        const tok = this.tokenStream.LT(2);
        if (!tok) return true;
        const sym = this.symTable.currentScope.lookupChain(tok.text ?? "");
        return sym === null || sym.kind !== Ng_CSharpSymbolKind.Variable;
    }

    IsTypeParameterName(): boolean {
        const tok = this.tokenStream.LT(1);
        if (!tok) return false;
        const sym = this.symTable.currentScope.lookupChain(tok.text ?? "");
        return sym !== null && sym.kind === Ng_CSharpSymbolKind.TypeParameter;
    }

    IsValueTypeName(): boolean {
        const tok = this.tokenStream.LT(1);
        if (!tok) return false;
        const vt = new Set<number>([CSharpLexer.KW_BOOL, CSharpLexer.KW_BYTE, CSharpLexer.KW_CHAR,
            CSharpLexer.KW_DECIMAL, CSharpLexer.KW_DOUBLE, CSharpLexer.KW_FLOAT,
            CSharpLexer.KW_INT, CSharpLexer.KW_LONG, CSharpLexer.KW_SBYTE,
            CSharpLexer.KW_SHORT, CSharpLexer.KW_UINT, CSharpLexer.KW_ULONG,
            CSharpLexer.KW_USHORT, CSharpLexer.TK_LPAREN]);
        if (vt.has(tok.type)) return true;
        const sym = this.symTable.currentScope.lookupChain(tok.text ?? "");
        if (!sym || sym.kind !== Ng_CSharpSymbolKind.Type) return false;
        return sym.typeKind === Ng_CSharpTypeKind.Struct || sym.typeKind === Ng_CSharpTypeKind.Enum;
    }

    IsReferenceTypeName(): boolean {
        const tok = this.tokenStream.LT(1);
        if (!tok) return true;
        const rt = new Set<number>([CSharpLexer.KW_DYNAMIC, CSharpLexer.KW_OBJECT, CSharpLexer.KW_STRING, CSharpLexer.TK_LBRACK]);
        if (rt.has(tok.type)) return true;
        const sym = this.symTable.currentScope.lookupChain(tok.text ?? "");
        if (!sym) return true;
        if (sym.kind === Ng_CSharpSymbolKind.TypeParameter) return false;
        if (sym.kind === Ng_CSharpSymbolKind.Type && (sym.typeKind === Ng_CSharpTypeKind.Struct || sym.typeKind === Ng_CSharpTypeKind.Enum)) return false;
        return true;
    }

    IsDelegateTypeName(): boolean {
        const tok = this.tokenStream.LT(1);
        if (!tok) return false;
        const sym = this.symTable.currentScope.lookupChain(tok.text ?? "");
        return sym !== null && sym.kind === Ng_CSharpSymbolKind.Type && sym.typeKind === Ng_CSharpTypeKind.Delegate;
    }

    IsInterfaceTypeName(): boolean {
        const tok = this.tokenStream.LT(1);
        if (!tok) return false;
        const sym = this.symTable.currentScope.lookupChain(tok.text ?? "");
        return sym !== null && sym.kind === Ng_CSharpSymbolKind.Type && sym.typeKind === Ng_CSharpTypeKind.Interface;
    }

    IsClassTypeName(): boolean {
        const tok = this.tokenStream.LT(1);
        if (!tok) return true;
        if (tok.type === CSharpLexer.KW_OBJECT || tok.type === CSharpLexer.KW_STRING) return true;
        const sym = this.symTable.currentScope.lookupChain(tok.text ?? "");
        if (!sym) return true;
        if (sym.kind !== Ng_CSharpSymbolKind.Type) return true;
        return sym.typeKind !== Ng_CSharpTypeKind.Interface && sym.typeKind !== Ng_CSharpTypeKind.Delegate;
    }

    private _classBaseTypeCheck(wantInterface: boolean): boolean {
        const tok = this.tokenStream.LT(2);
        if (!tok) return !wantInterface;
        if (tok.type === CSharpLexer.KW_OBJECT || tok.type === CSharpLexer.KW_STRING) return !wantInterface;
        const sym = this.symTable.currentScope.lookupChain(tok.text ?? "");
        if (!sym || sym.kind !== Ng_CSharpSymbolKind.Type) return !wantInterface;
        const isIface = sym.typeKind === Ng_CSharpTypeKind.Interface;
        return wantInterface ? isIface : !isIface;
    }

    IsClassBaseInterfaceList(): boolean { return this._classBaseTypeCheck(true); }
    IsClassBaseClassType():     boolean { return this._classBaseTypeCheck(false); }

    IsDeclarationPatternAhead(): boolean {
        // Heuristic (avoids stream corruption from Parser constructor's reset() → seek(0)):
        // A declaration pattern is "type simple_designation". We use token lookahead.
        const tok1 = this.tokenStream.LT(1);
        if (!tok1) return false;
        const vt = new Set<number>([CSharpLexer.KW_BOOL, CSharpLexer.KW_BYTE, CSharpLexer.KW_CHAR,
            CSharpLexer.KW_DECIMAL, CSharpLexer.KW_DOUBLE, CSharpLexer.KW_FLOAT,
            CSharpLexer.KW_INT, CSharpLexer.KW_LONG, CSharpLexer.KW_SBYTE,
            CSharpLexer.KW_SHORT, CSharpLexer.KW_UINT, CSharpLexer.KW_ULONG,
            CSharpLexer.KW_USHORT]);
        if (vt.has(tok1.type)) {
            const tok2 = this.tokenStream.LT(2);
            return tok2 !== null && (tok2.type === CSharpLexer.Simple_Identifier || tok2.text === "_");
        }
        if (tok1.type !== CSharpLexer.Simple_Identifier) return false;
        const sym = this.symTable.currentScope.lookupChain(tok1.text ?? "");
        const isKnownType = sym !== null && sym.kind === Ng_CSharpSymbolKind.Type;
        if (!isKnownType) return false;
        const tok2 = this.tokenStream.LT(2);
        return tok2 !== null && (tok2.type === CSharpLexer.Simple_Identifier || tok2.text === "_");
    }

    IsConstantPatternAhead(): boolean { return !this.IsDeclarationPatternAhead(); }

    IsImplicitlyTypedLocalVariable(): boolean {
        const tok = this.tokenStream.LT(1);
        if (!tok) return true;
        if (tok.type !== CSharpLexer.KW_VAR) return false;
        const sym = this.symTable.currentScope.lookupChain("var");
        if (sym && sym.kind === Ng_CSharpSymbolKind.Type) return false;
        const lt3 = this.tokenStream.LT(3);
        if (!lt3 || lt3.text !== "=") return false;
        const lt4 = this.tokenStream.LT(4);
        if (lt4 && lt4.text === "{") return false;
        return true;
    }

    IsExplicitlyTypedLocalVariable(): boolean {
        const tok = this.tokenStream.LT(1);
        if (!tok) return true;
        if (tok.type !== CSharpLexer.KW_VAR) return true;
        const sym = this.symTable.currentScope.lookupChain("var");
        if (sym && sym.kind === Ng_CSharpSymbolKind.Type) return true;
        const lt3 = this.tokenStream.LT(3);
        if (!lt3 || lt3.text !== "=") return true;
        const lt4 = this.tokenStream.LT(4);
        if (lt4 && lt4.text === "{") return true;
        return false;
    }

    IsExplicitlyTypedRefLocalVariable(): boolean {
        const tok = this.tokenStream.LT(1);
        return tok !== null && tok.type === CSharpLexer.KW_REF;
    }

    // Gates comma-separated declarators: false when type is 'var' (only one declarator allowed).
    protected IsLocalVariableDeclaration(): boolean {
        const local_var_decl = this.context as Local_variable_declarationContext | null;
        if (local_var_decl == null) return true;
        const local_variable_type = local_var_decl.local_variable_type();
        if (local_variable_type == null) return true;
        return local_variable_type.getText() !== "var";
    }
}

// =============================================================================
// Symbol table helpers (Ng prefix to avoid name collision)
// =============================================================================

export const enum Ng_CSharpSymbolKind { Type = "Type", Variable = "Variable", Namespace = "Namespace", TypeParameter = "TypeParameter", Alias = "Alias" }
export const enum Ng_CSharpTypeKind   { Class = "Class", Struct = "Struct", Interface = "Interface", Enum = "Enum", Delegate = "Delegate" }
export const enum Ng_CSharpScopeKind  { Global = "Global", Namespace = "Namespace", Type = "Type", Method = "Method", Block = "Block" }

export interface Ng_CSharpSymbol {
    name: string;
    kind: Ng_CSharpSymbolKind;
    typeKind?: Ng_CSharpTypeKind;
    typeRef?: string;
    target?: string;
}

export class Ng_CSharpScope {
    readonly kind: Ng_CSharpScopeKind;
    readonly parent: Ng_CSharpScope | null;
    readonly name: string;
    private readonly _symbols = new Map<string, Ng_CSharpSymbol>();

    constructor(kind: Ng_CSharpScopeKind, parent: Ng_CSharpScope | null, name = "") {
        this.kind = kind; this.parent = parent; this.name = name;
    }
    declare(sym: Ng_CSharpSymbol): void { this._symbols.set(sym.name, sym); }
    lookup(name: string): Ng_CSharpSymbol | null { return this._symbols.get(name) ?? null; }
    lookupChain(name: string): Ng_CSharpSymbol | null {
        const s = this.lookup(name);
        if (s !== null) return s;
        return this.parent ? this.parent.lookupChain(name) : null;
    }
}

const _CS_RESERVED_NG = new Set<string>([
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

function _ngIsIdentLike(t: string | undefined | null): boolean { if (!t) return false; return /^[A-Za-z_@]/.test(t); }

export class Ng_CSharpSymbolTable {
    private readonly _knownTypeNames = new Set<string>();
    private readonly _genericArities = new Map<string, Set<number>>();
    private readonly _scopeStack: Ng_CSharpScope[] = [];
    readonly globalScope: Ng_CSharpScope;

    constructor() {
        this.globalScope = new Ng_CSharpScope(Ng_CSharpScopeKind.Global, null, "<global>");
        this._scopeStack.push(this.globalScope);
        this._populateBuiltins();
    }

    get currentScope(): Ng_CSharpScope { return this._scopeStack[this._scopeStack.length - 1]; }

    enterScope(kind: Ng_CSharpScopeKind, name = ""): void {
        this._scopeStack.push(new Ng_CSharpScope(kind, this.currentScope, name));
    }
    exitScope(): void { if (this._scopeStack.length > 1) this._scopeStack.pop(); }

    declareType(name: string, kind: Ng_CSharpTypeKind, arity = 0): void {
        this.currentScope.declare({ name, kind: Ng_CSharpSymbolKind.Type, typeKind: kind });
        this._knownTypeNames.add(name);
        if (arity > 0) { if (!this._genericArities.has(name)) this._genericArities.set(name, new Set()); this._genericArities.get(name)!.add(arity); }
    }
    declareTypeParam(name: string): void {
        this.currentScope.declare({ name, kind: Ng_CSharpSymbolKind.TypeParameter });
        this._knownTypeNames.add(name);
    }
    declareVariable(name: string, typeRef: string): void {
        this.currentScope.declare({ name, kind: Ng_CSharpSymbolKind.Variable, typeRef });
    }
    declareAlias(alias: string, target: string): void {
        this.currentScope.declare({ name: alias, kind: Ng_CSharpSymbolKind.Alias, target });
        this._knownTypeNames.add(alias);
    }
    importNamespace(ns: string): void {
        this.currentScope.declare({ name: ns, kind: Ng_CSharpSymbolKind.Namespace });
    }

    preScan(tokens: any): void {
        try { tokens.fill(); } catch(e) {}
        const size: number = tokens.getNumberOfOnChannelTokens ? tokens.size : (tokens.tokens?.length ?? 0);
        const actualSize: number = (() => { try { return tokens.size; } catch(e) { return tokens.tokens?.length ?? 0; } })();
        const getN = (i: number) => { try { return tokens.get ? tokens.get(i) : (tokens.tokens ? tokens.tokens[i] : null); } catch(e) { return null; } };
        const sz = actualSize;
        const nextDefault = (pos: number): number => {
            while (pos < sz) { const t = getN(pos); if (!t || t.channel === 0) return pos; pos++; } return sz;
        };
        const skipAngled = (pos: number): number => {
            let depth = 1, i = nextDefault(pos + 1);
            while (i < sz && depth > 0) { const t = getN(i)?.text; if (t === '<') depth++; else if (t === '>') depth--; i = nextDefault(i + 1); } return i;
        };
        const countTypeParams = (pos: number): [number, string[]] => {
            let p = nextDefault(pos);
            if (p >= sz || getN(p)?.text !== '<') return [0, []];
            let depth = 1, arity = 1; const names: string[] = []; let i = nextDefault(p + 1);
            while (i < sz && depth > 0) {
                const t = getN(i)?.text;
                if (t === '<') depth++; else if (t === '>') depth--; else if (t === ',' && depth === 1) arity++; else if (depth === 1 && _ngIsIdentLike(t)) names.push(t!);
                i = nextDefault(i + 1);
            }
            return depth === 0 ? [arity, names] : [0, []];
        };
        const registerType = (name: string, arity: number, typeParams: string[]): void => {
            if (!_CS_RESERVED_NG.has(name)) this._knownTypeNames.add(name);
            if (arity > 0 && !_CS_RESERVED_NG.has(name)) { if (!this._genericArities.has(name)) this._genericArities.set(name, new Set()); this._genericArities.get(name)!.add(arity); }
            for (const tp of typeParams) if (_ngIsIdentLike(tp) && !_CS_RESERVED_NG.has(tp)) this._knownTypeNames.add(tp);
        };
        for (let i = 0; i < sz; i++) {
            const tok = getN(i);
            if (!tok || tok.channel !== 0) continue;
            const txt: string = tok.text;
            if (txt === 'class' || txt === 'struct' || txt === 'interface' || txt === 'enum') {
                let j = nextDefault(i + 1);
                while (j < sz && (getN(j)?.text === 'partial' || getN(j)?.text === 'ref')) j = nextDefault(j + 1);
                if (j < sz && _ngIsIdentLike(getN(j)?.text)) {
                    const [arity, params] = countTypeParams(j + 1);
                    registerType(getN(j).text, arity, params);
                }
            } else if (txt === 'delegate') {
                let j = nextDefault(i + 1);
                while (j < sz && (getN(j)?.text === 'ref' || getN(j)?.text === 'readonly')) j = nextDefault(j + 1);
                if (j >= sz) continue;
                j = nextDefault(j + 1);
                if (j < sz && getN(j)?.text === '<') j = skipAngled(j);
                while (j < sz && ['[', ']', ','].includes(getN(j)?.text)) j = nextDefault(j + 1);
                if (j < sz && getN(j)?.text === '?') j = nextDefault(j + 1);
                if (j < sz && _ngIsIdentLike(getN(j)?.text)) {
                    const [arity, params] = countTypeParams(j + 1);
                    registerType(getN(j).text, arity, params);
                }
            }
        }
    }

    private _populateBuiltins(): void {
        for (const t of ['bool','byte','sbyte','char','decimal','double','float','int','uint','long','ulong','short','ushort','object','string','void','dynamic',
            'Boolean','Byte','SByte','Char','Decimal','Double','Single','Int16','Int32','Int64','UInt16','UInt32','UInt64','IntPtr','UIntPtr','Object','String','Void',
            'Guid','DateTime','DateTimeOffset','TimeSpan','Uri','Exception','Type','Enum','Delegate','MulticastDelegate','Attribute','Math','Convert','Console','Environment',
            'List','IList','IEnumerable','IEnumerator','ICollection','IReadOnlyList','IReadOnlyCollection','IReadOnlyDictionary','Dictionary','IDictionary',
            'SortedDictionary','SortedList','HashSet','SortedSet','Queue','Stack','LinkedList','LinkedListNode','Func','Action','Predicate','Comparison','Converter',
            'Task','ValueTask','Nullable','IAsyncEnumerable','IAsyncEnumerator','Span','ReadOnlySpan','Memory','ReadOnlyMemory','KeyValuePair','Tuple',
            'ImmutableArray','ImmutableList','ImmutableDictionary','ImmutableHashSet','Lazy','WeakReference','EventHandler',
            'IEqualityComparer','IComparer','EqualityComparer','Comparer','ConcurrentDictionary','ConcurrentQueue','ConcurrentStack','ConcurrentBag'])
            this._knownTypeNames.add(t);
    }
}
