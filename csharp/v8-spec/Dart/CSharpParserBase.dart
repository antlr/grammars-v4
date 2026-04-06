import 'dart:io';
import 'package:antlr4/antlr4.dart';
import 'CSharpLexer.dart';
import 'CSharpParser.dart';

abstract class CSharpParserBase extends Parser {

    CSharpParserBase(TokenStream input) : super(input);

    // -------------------------------------------------------------------------
    // Look-ahead helpers
    // -------------------------------------------------------------------------

    bool LookAheadIs(int pos, int value) =>
        (inputStream as TokenStream).LT(pos)?.type == value;

    bool LookAheadIsNot(int pos, int value) =>
        (inputStream as TokenStream).LT(pos)?.type != value;

    // -------------------------------------------------------------------------
    // Error reporting
    // -------------------------------------------------------------------------

    void _notifySemanticError(int line, int charPositionInLine, String msg) {
        notifyErrorListeners(msg);
    }

    // -------------------------------------------------------------------------
    // ReduceTree — post-parse cleanup (for external tooling)
    // -------------------------------------------------------------------------

    void ReduceTree(ParserRuleContext currentctx) {
        final ruleTypeArgumentList = ruleNames.indexOf("type_argument_list");

        void takeOutEmpties(ParserRuleContext node) {
            final children = node.children;
            if (children == null) return;
            for (var ix = children.length - 1; ix >= 0; ix--) {
                final child = children[ix];
                if (child is ParserRuleContext) {
                    if (child.ruleIndex == ruleTypeArgumentList && child.childCount == 0)
                        children.removeAt(ix);
                    else
                        takeOutEmpties(child);
                }
            }
        }
        takeOutEmpties(currentctx);

        final reduceTree = Platform.environment['ANTLR_REDUCE_TREE'] == 'yes';
        if (!reduceTree) return;

        final reduceAllChildren = Platform.environment['ANTLR_REDUCE_ALL_CHILDREN'] == 'yes';

        void reducer(ParserRuleContext node) {
            final children = node.children;
            if (children == null) return;
            for (final child in children)
                if (child is ParserRuleContext) reducer(child);
            if (reduceAllChildren || node.childCount == 1) {
                for (var i = 0; i < children.length; i++) {
                    final child = children[i];
                    if (child is ParserRuleContext && child.childCount == 1) {
                        final grandchild = child.getChild(0);
                        if (grandchild is ParserRuleContext)
                            children[i] = grandchild;
                    }
                }
            }
        }
        if (currentctx.children != null) reducer(currentctx);
    }

    // -------------------------------------------------------------------------
    // _insertNode — rewire parse tree (MLR un-inlining)
    // NOTE: Dart does not support runtime type lookup by name without dart:mirrors.
    // This no-op implementation still allows parsing to succeed; tree structure
    // will be flat rather than nested. Implement via dart:mirrors if needed.
    // -------------------------------------------------------------------------

    void _insertNode(ParserRuleContext currentctx, String contextTypeName) {
        // no-op: tree restructuring not supported without dart:mirrors
    }

    void AsInvocationExpression(ParserRuleContext currentctx)         => _insertNode(currentctx, "Invocation_expressionContext");
    void AsElementAccess(ParserRuleContext currentctx)                => _insertNode(currentctx, "Element_accessContext");
    void AsMemberAccess(ParserRuleContext currentctx)                 => _insertNode(currentctx, "Member_accessContext");
    void AsNullConditionalMemberAccess(ParserRuleContext currentctx)  => _insertNode(currentctx, "Null_conditional_member_accessContext");
    void AsNullConditionalElementAccess(ParserRuleContext currentctx) => _insertNode(currentctx, "Null_conditional_element_accessContext");
    void AsPostIncrementExpression(ParserRuleContext currentctx)      => _insertNode(currentctx, "Post_increment_expressionContext");
    void AsPostDecrementExpression(ParserRuleContext currentctx)      => _insertNode(currentctx, "Post_decrement_expressionContext");
    void AsNullForgivingExpression(ParserRuleContext currentctx)      => _insertNode(currentctx, "Null_forgiving_expressionContext");
    void AsPointerMemberAccess(ParserRuleContext currentctx)          => _insertNode(currentctx, "Pointer_member_accessContext");

    // -------------------------------------------------------------------------
    // ElementAccessSemanticCheck
    // -------------------------------------------------------------------------

    void ElementAccessSemanticCheck(ParserRuleContext currentctx) {
        int rIdx(String name) => ruleNames.indexOf(name);

        final rulePrimaryExpression            = rIdx("primary_expression");
        final ruleElementAccess                = rIdx("element_access");
        final rulePointerElementAccess         = rIdx("pointer_element_access");
        final ruleNullConditionalElementAccess = rIdx("null_conditional_element_access");
        final ruleArrayCreationExpression      = rIdx("array_creation_expression");
        final ruleStackallocExpression         = rIdx("stackalloc_expression");

        if (currentctx.ruleIndex != rulePrimaryExpression || currentctx.childCount != 1) return;

        final childRule = currentctx.getChild(0);
        if (childRule is! ParserRuleContext) return;

        final childRuleIndex  = childRule.ruleIndex;
        final childChildCount = childRule.childCount;

        if (childRuleIndex == ruleElementAccess || childRuleIndex == rulePointerElementAccess) {
            if (childChildCount != 4) return;
        } else if (childRuleIndex == ruleNullConditionalElementAccess) {
            if (childChildCount != 5) return;
        } else return;

        final accessTarget = childRule.getChild(0);
        if (accessTarget is! ParserRuleContext) return;
        if (accessTarget.ruleIndex != rulePrimaryExpression || accessTarget.childCount == 0) return;

        final lhsTarget = accessTarget.getChild(0);
        if (lhsTarget is! ParserRuleContext) return;

        if (lhsTarget.ruleIndex != ruleArrayCreationExpression
                && lhsTarget.ruleIndex != ruleStackallocExpression) return;

        final lhsLast     = lhsTarget.stop;
        final lhsLastType = lhsLast?.type ?? -1;

        if (lhsLastType == CSharpLexer.TOKEN_TK_RBRACE) return; // initializer present

        if (lhsLastType != CSharpLexer.TOKEN_TK_RBRACK) {
            stderr.writeln('${lhsLast?.line}:${lhsLast?.charPositionInLine} '
                'Error: Unexpected LHS last token ${lhsLast?.text} ($lhsLastType).');
            return;
        }

        final childRuleName = ruleNames[childRuleIndex];
        final lhsRuleName   = ruleNames[lhsTarget.ruleIndex];
        final childPrefix   = 'AEIOUaeiou'.contains(childRuleName[0]) ? 'an' : 'a';
        final lhsPrefix     = 'AEIOUaeiou'.contains(lhsRuleName[0])   ? 'an' : 'a';

        _notifySemanticError(lhsLast?.line ?? 0, lhsLast?.charPositionInLine ?? 0,
            'LHS of $childPrefix $childRuleName cannot be $lhsPrefix $lhsRuleName unless it has an initializer');
    }

    // =========================================================================
    // Symbol table
    // =========================================================================

    CSharpSymbolTable? _symTable;
    String _pendingVarType = '?';

    CSharpSymbolTable get symTable {
        _symTable ??= CSharpSymbolTable();
        return _symTable!;
    }

    void EnterNamespaceScope() => symTable.enterScope(CSharpScopeKind.namesp);
    void EnterTypeScope()      => symTable.enterScope(CSharpScopeKind.type_);
    void EnterBlockScope()     => symTable.enterScope(CSharpScopeKind.block);
    void ExitCurrentScope()    => symTable.exitScope();

    void OnTypeParameter() {
        final ctx = context!;
        final name = ctx.getChild(ctx.childCount - 1)!.text!;
        symTable.declareTypeParam(name);
    }

    void OnUsingAliasDirective() {
        final ctx = context!;
        final alias  = ctx.getChild(1)!.text!;
        final target = ctx.getChild(3)!.text!;
        symTable.declareAlias(alias, target);
    }

    void OnUsingNamespaceDirective() {
        final ctx = context!;
        final ns = ctx.getChild(1)!.text!;
        symTable.importNamespace(ns);
    }

    void BeginVariableDeclaration() {
        final ctx = context!;
        _pendingVarType = ctx.getChild(ctx.childCount - 1)!.text!;
    }

    void OnVariableDeclarator() {
        final ctx = context!;
        final id = ctx.getChild(ctx.childCount - 1)!.text!;
        symTable.declareVariable(id, _pendingVarType);
    }

    bool IsCastExpressionAhead() {
        final tok = (inputStream as TokenStream).LT(2);
        if (tok == null) return true;
        final sym = symTable.currentScope.lookupChain(tok.text ?? '');
        return sym == null || sym.kind != CSharpSymbolKind.variable;
    }

    bool IsTypeParameterName() {
        final tok = (inputStream as TokenStream).LT(1);
        if (tok == null) return false;
        final sym = symTable.currentScope.lookupChain(tok.text ?? '');
        return sym != null && sym.kind == CSharpSymbolKind.typeParameter;
    }

    bool IsValueTypeName() {
        final tok = (inputStream as TokenStream).LT(1);
        if (tok == null) return false;
        const valueTypeTokens = <int>{
            CSharpLexer.TOKEN_KW_BOOL,   CSharpLexer.TOKEN_KW_BYTE,
            CSharpLexer.TOKEN_KW_CHAR,   CSharpLexer.TOKEN_KW_DECIMAL,
            CSharpLexer.TOKEN_KW_DOUBLE, CSharpLexer.TOKEN_KW_FLOAT,
            CSharpLexer.TOKEN_KW_INT,    CSharpLexer.TOKEN_KW_LONG,
            CSharpLexer.TOKEN_KW_SBYTE,  CSharpLexer.TOKEN_KW_SHORT,
            CSharpLexer.TOKEN_KW_UINT,   CSharpLexer.TOKEN_KW_ULONG,
            CSharpLexer.TOKEN_KW_USHORT, CSharpLexer.TOKEN_TK_LPAREN,
        };
        if (valueTypeTokens.contains(tok.type)) return true;
        final sym = symTable.currentScope.lookupChain(tok.text ?? '');
        if (sym == null || sym.kind != CSharpSymbolKind.type_) return false;
        return sym.typeKind == CSharpTypeKind.struct_ || sym.typeKind == CSharpTypeKind.enum_;
    }

    bool IsReferenceTypeName() {
        final tok = (inputStream as TokenStream).LT(1);
        if (tok == null) return true;
        const refTypeTokens = <int>{
            CSharpLexer.TOKEN_KW_DYNAMIC, CSharpLexer.TOKEN_KW_OBJECT,
            CSharpLexer.TOKEN_KW_STRING,  CSharpLexer.TOKEN_TK_LBRACK,
        };
        if (refTypeTokens.contains(tok.type)) return true;
        final sym = symTable.currentScope.lookupChain(tok.text ?? '');
        if (sym == null) return true;
        if (sym.kind == CSharpSymbolKind.typeParameter) return false;
        if (sym.kind == CSharpSymbolKind.type_ &&
            (sym.typeKind == CSharpTypeKind.struct_ || sym.typeKind == CSharpTypeKind.enum_)) return false;
        return true;
    }

    bool IsDelegateTypeName() {
        final tok = (inputStream as TokenStream).LT(1);
        if (tok == null) return false;
        final sym = symTable.currentScope.lookupChain(tok.text ?? '');
        return sym != null && sym.kind == CSharpSymbolKind.type_ && sym.typeKind == CSharpTypeKind.delegate_;
    }

    bool IsInterfaceTypeName() {
        final tok = (inputStream as TokenStream).LT(1);
        if (tok == null) return false;
        final sym = symTable.currentScope.lookupChain(tok.text ?? '');
        return sym != null && sym.kind == CSharpSymbolKind.type_ && sym.typeKind == CSharpTypeKind.interface_;
    }

    bool IsClassTypeName() {
        final tok = (inputStream as TokenStream).LT(1);
        if (tok == null) return true;
        if (tok.type == CSharpLexer.TOKEN_KW_OBJECT || tok.type == CSharpLexer.TOKEN_KW_STRING) return true;
        final sym = symTable.currentScope.lookupChain(tok.text ?? '');
        if (sym == null) return true;
        if (sym.kind != CSharpSymbolKind.type_) return true;
        return sym.typeKind != CSharpTypeKind.interface_ && sym.typeKind != CSharpTypeKind.delegate_;
    }

    bool _classBaseTypeCheck(bool wantInterface) {
        final tok = (inputStream as TokenStream).LT(2);
        if (tok == null) return !wantInterface;
        if (tok.type == CSharpLexer.TOKEN_KW_OBJECT || tok.type == CSharpLexer.TOKEN_KW_STRING)
            return !wantInterface;
        final sym = symTable.currentScope.lookupChain(tok.text ?? '');
        if (sym == null || sym.kind != CSharpSymbolKind.type_) return !wantInterface;
        final isIface = sym.typeKind == CSharpTypeKind.interface_;
        return wantInterface ? isIface : !isIface;
    }

    bool IsClassBaseInterfaceList() => _classBaseTypeCheck(true);
    bool IsClassBaseClassType()     => _classBaseTypeCheck(false);

    // Speculative parse not easily supported in Dart; use token-lookahead heuristic.
    // A declaration pattern starts with a type name (identifier or keyword) followed
    // by a simple identifier designator. We check: LT(1) is a known type name and
    // LT(2) is Simple_Identifier or '_'.
    bool IsDeclarationPatternAhead() {
        final tok1 = (inputStream as TokenStream).LT(1);
        if (tok1 == null) return false;
        // value-type keywords always start a declaration pattern
        const valueTypeTokens = <int>{
            CSharpLexer.TOKEN_KW_BOOL,   CSharpLexer.TOKEN_KW_BYTE,
            CSharpLexer.TOKEN_KW_CHAR,   CSharpLexer.TOKEN_KW_DECIMAL,
            CSharpLexer.TOKEN_KW_DOUBLE, CSharpLexer.TOKEN_KW_FLOAT,
            CSharpLexer.TOKEN_KW_INT,    CSharpLexer.TOKEN_KW_LONG,
            CSharpLexer.TOKEN_KW_SBYTE,  CSharpLexer.TOKEN_KW_SHORT,
            CSharpLexer.TOKEN_KW_UINT,   CSharpLexer.TOKEN_KW_ULONG,
            CSharpLexer.TOKEN_KW_USHORT,
        };
        bool couldBeType = valueTypeTokens.contains(tok1.type) ||
            (tok1.type == CSharpLexer.TOKEN_Simple_Identifier &&
             symTable.currentScope.lookupChain(tok1.text ?? '') != null);
        if (!couldBeType) return false;
        final tok2 = (inputStream as TokenStream).LT(2);
        return tok2 != null &&
            (tok2.type == CSharpLexer.TOKEN_Simple_Identifier || tok2.text == '_');
    }

    bool IsConstantPatternAhead() => !IsDeclarationPatternAhead();

    bool IsImplicitlyTypedLocalVariable() {
        final tok = (inputStream as TokenStream).LT(1);
        if (tok == null) return true;
        if (tok.type != CSharpLexer.TOKEN_KW_VAR) return false;
        final sym = symTable.currentScope.lookupChain('var');
        if (sym != null && sym.kind == CSharpSymbolKind.type_) return false;
        final lt3 = (inputStream as TokenStream).LT(3);
        if (lt3 == null || lt3.text != '=') return false;
        final lt4 = (inputStream as TokenStream).LT(4);
        if (lt4 != null && lt4.text == '{') return false;
        return true;
    }

    bool IsExplicitlyTypedLocalVariable() {
        final tok = (inputStream as TokenStream).LT(1);
        if (tok == null) return true;
        if (tok.type != CSharpLexer.TOKEN_KW_VAR) return true;
        final sym = symTable.currentScope.lookupChain('var');
        if (sym != null && sym.kind == CSharpSymbolKind.type_) return true;
        final lt3 = (inputStream as TokenStream).LT(3);
        if (lt3 == null || lt3.text != '=') return true;
        final lt4 = (inputStream as TokenStream).LT(4);
        if (lt4 != null && lt4.text == '{') return true;
        return false;
    }

    bool IsExplicitlyTypedRefLocalVariable() {
        final tok = (inputStream as TokenStream).LT(1);
        return tok != null && tok.type == CSharpLexer.TOKEN_KW_REF;
    }

    // Gates comma-separated declarators: false when type is 'var' (only one declarator allowed).
    bool IsLocalVariableDeclaration() {
        final local_var_decl = context;
        if (local_var_decl is! Local_variable_declarationContext) return true;
        final local_variable_type = local_var_decl.local_variable_type();
        if (local_variable_type == null) return true;
        return local_variable_type.text != "var";
    }
}

// =============================================================================
// Symbol table helpers
// =============================================================================

enum CSharpSymbolKind { type_, variable, namespace_, typeParameter, alias_ }
enum CSharpTypeKind   { class_, struct_, interface_, enum_, delegate_ }
enum CSharpScopeKind  { global, namesp, type_, method, block }

class CSharpSymbol {
    final String name;
    final CSharpSymbolKind kind;
    final CSharpTypeKind? typeKind;
    final String? typeRef;
    final String? target;
    CSharpSymbol(this.name, this.kind, {this.typeKind, this.typeRef, this.target});
}

class CSharpScope {
    final CSharpScopeKind kind;
    final CSharpScope? parent;
    final String name;
    final Map<String, CSharpSymbol> _symbols = {};

    CSharpScope(this.kind, this.parent, [this.name = '']);

    void declare(CSharpSymbol sym) => _symbols[sym.name] = sym;
    CSharpSymbol? lookup(String n) => _symbols[n];
    CSharpSymbol? lookupChain(String n) => lookup(n) ?? parent?.lookupChain(n);
}

const _dartCsReserved = <String>{
    'abstract','as','base','bool','break','byte','case','catch','char','checked',
    'class','const','continue','decimal','default','delegate','do','double','else',
    'enum','event','explicit','extern','false','finally','fixed','float','for',
    'foreach','goto','if','implicit','in','int','interface','internal','is','lock',
    'long','namespace','new','null','object','operator','out','override','params',
    'private','protected','public','readonly','ref','return','sbyte','sealed','short',
    'sizeof','stackalloc','static','string','struct','switch','this','throw','true',
    'try','typeof','uint','ulong','unchecked','unsafe','ushort','using','virtual',
    'void','volatile','while',
};

bool _dartIsIdentLike(String? t) {
    if (t == null || t.isEmpty) return false;
    final c = t[0];
    return RegExp(r'^[A-Za-z_@]').hasMatch(c);
}

class CSharpSymbolTable {
    final Set<String> _knownTypeNames = {};
    final Map<String, Set<int>> _genericArities = {};
    final List<CSharpScope> _scopeStack = [];
    late final CSharpScope globalScope;

    CSharpSymbolTable() {
        globalScope = CSharpScope(CSharpScopeKind.global, null, '<global>');
        _scopeStack.add(globalScope);
        _populateBuiltins();
    }

    CSharpScope get currentScope => _scopeStack.last;

    void enterScope(CSharpScopeKind kind, [String name = '']) =>
        _scopeStack.add(CSharpScope(kind, currentScope, name));
    void exitScope() { if (_scopeStack.length > 1) _scopeStack.removeLast(); }

    void declareType(String name, CSharpTypeKind kind, [int arity = 0]) {
        currentScope.declare(CSharpSymbol(name, CSharpSymbolKind.type_, typeKind: kind));
        _knownTypeNames.add(name);
        if (arity > 0) (_genericArities[name] ??= {}).add(arity);
    }
    void declareTypeParam(String name) {
        currentScope.declare(CSharpSymbol(name, CSharpSymbolKind.typeParameter));
        _knownTypeNames.add(name);
    }
    void declareVariable(String name, String typeRef) =>
        currentScope.declare(CSharpSymbol(name, CSharpSymbolKind.variable, typeRef: typeRef));
    void declareAlias(String alias, String target) {
        currentScope.declare(CSharpSymbol(alias, CSharpSymbolKind.alias_, target: target));
        _knownTypeNames.add(alias);
    }
    void importNamespace(String ns) =>
        currentScope.declare(CSharpSymbol(ns, CSharpSymbolKind.namespace_));

    bool isTypeName(String name) {
        final sym = currentScope.lookupChain(name);
        if (sym != null)
            return sym.kind == CSharpSymbolKind.type_ ||
                   sym.kind == CSharpSymbolKind.typeParameter ||
                   sym.kind == CSharpSymbolKind.alias_;
        return _knownTypeNames.contains(name);
    }

    void preScan(TokenStream tokens) {
        try { (tokens as dynamic).fill(); } catch(_) {}
        final n = tokens.size;
        int nextDefault(int pos) {
            while (pos < n) { final t = tokens.get(pos); if (t == null || t.channel == 0) return pos; pos++; } return n;
        }
        int skipAngled(int pos) {
            int depth = 1, i = nextDefault(pos + 1);
            while (i < n && depth > 0) { final t = tokens.get(i).text; if (t == '<') depth++; else if (t == '>') depth--; i = nextDefault(i + 1); } return i;
        }
        // Returns [arity, List<String> typeParamNames]
        List<Object> countTypeParams(int pos) {
            final p = nextDefault(pos);
            if (p >= n || tokens.get(p).text != '<') return [0, <String>[]];
            int depth = 1, arity = 1; final names = <String>[]; int i = nextDefault(p + 1);
            while (i < n && depth > 0) {
                final t = tokens.get(i).text;
                if (t == '<') depth++; else if (t == '>') depth--; else if (t == ',' && depth == 1) arity++; else if (depth == 1 && _dartIsIdentLike(t)) names.add(t!);
                i = nextDefault(i + 1);
            }
            return depth == 0 ? [arity, names] : [0, <String>[]];
        }
        void registerType(String name, int arity, List<String> typeParams) {
            if (!_dartCsReserved.contains(name)) _knownTypeNames.add(name);
            if (arity > 0 && !_dartCsReserved.contains(name)) (_genericArities[name] ??= {}).add(arity);
            for (final tp in typeParams) if (_dartIsIdentLike(tp) && !_dartCsReserved.contains(tp)) _knownTypeNames.add(tp);
        }
        for (int i = 0; i < n; i++) {
            final tok = tokens.get(i);
            if (tok == null || tok.channel != 0) continue;
            final txt = tok.text;
            if (txt == 'class' || txt == 'struct' || txt == 'interface' || txt == 'enum') {
                int j = nextDefault(i + 1);
                while (j < n && (tokens.get(j).text == 'partial' || tokens.get(j).text == 'ref')) j = nextDefault(j + 1);
                if (j < n && _dartIsIdentLike(tokens.get(j).text)) {
                    final r = countTypeParams(j + 1);
                    registerType(tokens.get(j).text!, r[0] as int, r[1] as List<String>);
                }
            } else if (txt == 'delegate') {
                int j = nextDefault(i + 1);
                while (j < n && (tokens.get(j).text == 'ref' || tokens.get(j).text == 'readonly')) j = nextDefault(j + 1);
                if (j >= n) continue;
                j = nextDefault(j + 1);
                if (j < n && tokens.get(j).text == '<') j = skipAngled(j);
                while (j < n && <String>['[',']',','].contains(tokens.get(j).text)) j = nextDefault(j + 1);
                if (j < n && tokens.get(j).text == '?') j = nextDefault(j + 1);
                if (j < n && _dartIsIdentLike(tokens.get(j).text)) {
                    final r = countTypeParams(j + 1);
                    registerType(tokens.get(j).text!, r[0] as int, r[1] as List<String>);
                }
            }
        }
    }

    void _populateBuiltins() {
        for (final t in const [
            'bool','byte','sbyte','char','decimal','double','float','int','uint','long','ulong','short','ushort','object','string','void','dynamic',
            'Boolean','Byte','SByte','Char','Decimal','Double','Single','Int16','Int32','Int64','UInt16','UInt32','UInt64','IntPtr','UIntPtr','Object','String','Void',
            'Guid','DateTime','DateTimeOffset','TimeSpan','Uri','Exception','Type','Enum','Delegate','MulticastDelegate','Attribute','Math','Convert','Console','Environment',
            'List','IList','IEnumerable','IEnumerator','ICollection','IReadOnlyList','IReadOnlyCollection','IReadOnlyDictionary','Dictionary','IDictionary',
            'SortedDictionary','SortedList','HashSet','SortedSet','Queue','Stack','LinkedList','LinkedListNode','Func','Action','Predicate','Comparison','Converter',
            'Task','ValueTask','Nullable','IAsyncEnumerable','IAsyncEnumerator','Span','ReadOnlySpan','Memory','ReadOnlyMemory','KeyValuePair','Tuple',
            'ImmutableArray','ImmutableList','ImmutableDictionary','ImmutableHashSet','Lazy','WeakReference','EventHandler',
            'IEqualityComparer','IComparer','EqualityComparer','Comparer','ConcurrentDictionary','ConcurrentQueue','ConcurrentStack','ConcurrentBag'])
            _knownTypeNames.add(t);
    }
}
