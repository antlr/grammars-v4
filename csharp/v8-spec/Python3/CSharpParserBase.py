import sys
import os
from antlr4 import Parser, ParserRuleContext


class CSharpParserBase(Parser):

    def __init__(self, input, output=sys.stdout):
        super().__init__(input, output)
        self._sym_table = None
        self._pending_var_type = "?"

    # -------------------------------------------------------------------------
    # Symbol table — lazily initialised
    # -------------------------------------------------------------------------

    @property
    def SymTable(self):
        if self._sym_table is None:
            self._sym_table = CSharpSymbolTable()
        return self._sym_table

    # -------------------------------------------------------------------------
    # Look-ahead helpers
    # -------------------------------------------------------------------------

    def LookAheadIs(self, pos, value):
        return self._input.LA(pos) == value

    def LookAheadIsNot(self, pos, value):
        return self._input.LA(pos) != value

    # -------------------------------------------------------------------------
    # Error reporting
    # -------------------------------------------------------------------------

    def _notifySemanticError(self, line, charPositionInLine, msg):
        self.notifyErrorListeners(msg)

    # -------------------------------------------------------------------------
    # ReduceTree — post-parse cleanup (for external tooling)
    # -------------------------------------------------------------------------

    def ReduceTree(self, currentctx):
        try:
            ruleTypeArgumentList = list(self.ruleNames).index("type_argument_list")
        except ValueError:
            return

        def takeOutEmpties(node):
            if not isinstance(node, ParserRuleContext) or node.children is None:
                return
            i = len(node.children) - 1
            while i >= 0:
                child = node.children[i]
                if isinstance(child, ParserRuleContext):
                    if child.getRuleIndex() == ruleTypeArgumentList and child.getChildCount() == 0:
                        node.children.pop(i)
                    else:
                        takeOutEmpties(child)
                i -= 1

        takeOutEmpties(currentctx)

        reduceProperty = os.environ.get("ANTLR_REDUCE_TREE", "no").lower()
        if reduceProperty != "yes":
            return

        reduceAllChildren = os.environ.get("ANTLR_REDUCE_ALL_CHILDREN", "no").lower() == "yes"

        def reducer(node):
            if not isinstance(node, ParserRuleContext) or node.children is None:
                return
            for child in node.children:
                reducer(child)
            if reduceAllChildren or node.getChildCount() == 1:
                for i in range(len(node.children)):
                    child = node.children[i]
                    if isinstance(child, ParserRuleContext) and child.getChildCount() == 1:
                        grandchild = child.getChild(0)
                        if isinstance(grandchild, ParserRuleContext):
                            node.children[i] = grandchild

        if currentctx.children is not None:
            reducer(currentctx)

    # -------------------------------------------------------------------------
    # InsertNode — rewire parse tree (MLR un-inlining)
    # -------------------------------------------------------------------------

    def _insertNode(self, currentctx, contextTypeName):
        return
        ctx_class = getattr(type(self), contextTypeName, None)
        if ctx_class is None:
            return
        inserted = ctx_class(self, currentctx, currentctx.invokingState)
        inserted.children = list(currentctx.children) if currentctx.children else []
        if currentctx.children is None:
            currentctx.children = []
        currentctx.children.clear()
        currentctx.children.append(inserted)

    def AsInvocationExpression(self, currentctx):
        self._insertNode(currentctx, "Invocation_expressionContext")

    def AsElementAccess(self, currentctx):
        self._insertNode(currentctx, "Element_accessContext")

    def AsMemberAccess(self, currentctx):
        self._insertNode(currentctx, "Member_accessContext")

    def AsNullConditionalMemberAccess(self, currentctx):
        self._insertNode(currentctx, "Null_conditional_member_accessContext")

    def AsNullConditionalElementAccess(self, currentctx):
        self._insertNode(currentctx, "Null_conditional_element_accessContext")

    def AsPostIncrementExpression(self, currentctx):
        self._insertNode(currentctx, "Post_increment_expressionContext")

    def AsPostDecrementExpression(self, currentctx):
        self._insertNode(currentctx, "Post_decrement_expressionContext")

    def AsNullForgivingExpression(self, currentctx):
        self._insertNode(currentctx, "Null_forgiving_expressionContext")

    def AsPointerMemberAccess(self, currentctx):
        self._insertNode(currentctx, "Pointer_member_accessContext")

    # -------------------------------------------------------------------------
    # ElementAccessSemanticCheck
    # -------------------------------------------------------------------------

    def ElementAccessSemanticCheck(self, currentctx):
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
        else:
            from CSharpLexer import CSharpLexer

        ruleNames = list(self.ruleNames)

        def ruleIdx(name):
            try:
                return ruleNames.index(name)
            except ValueError:
                return -1

        rulePrimaryExpression            = ruleIdx("primary_expression")
        ruleElementAccess                = ruleIdx("element_access")
        rulePointerElementAccess         = ruleIdx("pointer_element_access")
        ruleNullConditionalElementAccess = ruleIdx("null_conditional_element_access")
        ruleArrayCreationExpression      = ruleIdx("array_creation_expression")
        ruleStackallocExpression         = ruleIdx("stackalloc_expression")

        if not isinstance(currentctx, ParserRuleContext):
            return
        if currentctx.getRuleIndex() != rulePrimaryExpression or currentctx.getChildCount() != 1:
            return

        childRule = currentctx.getChild(0)
        if not isinstance(childRule, ParserRuleContext):
            return

        childRuleIndex  = childRule.getRuleIndex()
        childChildCount = childRule.getChildCount()

        if childRuleIndex == ruleElementAccess or childRuleIndex == rulePointerElementAccess:
            if childChildCount != 4:
                return
        elif childRuleIndex == ruleNullConditionalElementAccess:
            if childChildCount != 5:
                return
        else:
            return

        accessTarget = childRule.getChild(0)
        if not isinstance(accessTarget, ParserRuleContext):
            return
        if accessTarget.getRuleIndex() != rulePrimaryExpression or accessTarget.getChildCount() == 0:
            return

        lhsTarget = accessTarget.getChild(0)
        if not isinstance(lhsTarget, ParserRuleContext):
            return

        if lhsTarget.getRuleIndex() not in (ruleArrayCreationExpression, ruleStackallocExpression):
            return

        lhsLast     = lhsTarget.stop
        lhsLastType = lhsLast.type if lhsLast else -1

        if lhsLastType == CSharpLexer.TK_RBRACE:
            return

        if lhsLastType != CSharpLexer.TK_RBRACK:
            print(f"{lhsLast.line}:{lhsLast.column} Error: Unexpected LHS last token {lhsLast.text} ({lhsLastType}).",
                  file=sys.stderr)
            return

        childRuleName = self.ruleNames[childRuleIndex]
        lhsRuleName   = self.ruleNames[lhsTarget.getRuleIndex()]
        childPrefix   = "an" if childRuleName[0] in "AEIOUaeiou" else "a"
        lhsPrefix     = "an" if lhsRuleName[0]   in "AEIOUaeiou" else "a"

        self.notifyErrorListeners(
            f"LHS of {childPrefix} {childRuleName} cannot be {lhsPrefix} {lhsRuleName} unless it has an initializer")

    # =========================================================================
    # Scope management
    # =========================================================================

    def EnterNamespaceScope(self):
        self.SymTable.enter_scope(CSharpScopeKind.Namespace)

    def EnterTypeScope(self):
        self.SymTable.enter_scope(CSharpScopeKind.Type)

    def EnterBlockScope(self):
        self.SymTable.enter_scope(CSharpScopeKind.Block)

    def ExitCurrentScope(self):
        self.SymTable.exit_scope()

    # =========================================================================
    # Grammar actions — using directive, type parameter, variable declarator
    # =========================================================================

    def OnTypeParameter(self):
        ctx = self._ctx
        name = ctx.getChild(ctx.getChildCount() - 1).getText()
        self.SymTable.declare_type_param(name)

    def OnUsingAliasDirective(self):
        ctx = self._ctx
        alias  = ctx.getChild(1).getText()
        target = ctx.getChild(3).getText()
        self.SymTable.declare_alias(alias, target)

    def OnUsingNamespaceDirective(self):
        ctx = self._ctx
        ns = ctx.getChild(1).getText()
        self.SymTable.import_namespace(ns)

    def BeginVariableDeclaration(self):
        ctx = self._ctx
        self._pending_var_type = ctx.getChild(ctx.getChildCount() - 1).getText()

    def OnVariableDeclarator(self):
        ctx = self._ctx
        ident = ctx.getChild(ctx.getChildCount() - 1).getText()
        self.SymTable.declare_variable(ident, self._pending_var_type)

    # =========================================================================
    # Semantic predicates
    # =========================================================================

    def IsCastExpressionAhead(self):
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
        else:
            from CSharpLexer import CSharpLexer
        tok = self._input.LT(2)
        if tok is None:
            return True
        sym = self.SymTable.current_scope.lookup_chain(tok.text)
        return sym is None or sym.kind != CSharpSymbolKind.Variable

    def IsTypeParameterName(self):
        tok = self._input.LT(1)
        if tok is None:
            return False
        sym = self.SymTable.current_scope.lookup_chain(tok.text)
        return sym is not None and sym.kind == CSharpSymbolKind.TypeParameter

    def IsValueTypeName(self):
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
        else:
            from CSharpLexer import CSharpLexer
        tok = self._input.LT(1)
        if tok is None:
            return False
        value_type_tokens = {
            CSharpLexer.KW_BOOL, CSharpLexer.KW_BYTE, CSharpLexer.KW_CHAR,
            CSharpLexer.KW_DECIMAL, CSharpLexer.KW_DOUBLE, CSharpLexer.KW_FLOAT,
            CSharpLexer.KW_INT, CSharpLexer.KW_LONG, CSharpLexer.KW_SBYTE,
            CSharpLexer.KW_SHORT, CSharpLexer.KW_UINT, CSharpLexer.KW_ULONG,
            CSharpLexer.KW_USHORT, CSharpLexer.TK_LPAREN,
        }
        if tok.type in value_type_tokens:
            return True
        sym = self.SymTable.current_scope.lookup_chain(tok.text)
        if sym is None or sym.kind != CSharpSymbolKind.Type:
            return False
        return isinstance(sym, CSharpTypeSymbol) and sym.type_kind in (CSharpTypeKind.Struct, CSharpTypeKind.Enum)

    def IsReferenceTypeName(self):
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
        else:
            from CSharpLexer import CSharpLexer
        tok = self._input.LT(1)
        if tok is None:
            return True
        if tok.type in (CSharpLexer.KW_DYNAMIC, CSharpLexer.KW_OBJECT,
                        CSharpLexer.KW_STRING, CSharpLexer.TK_LBRACK):
            return True
        sym = self.SymTable.current_scope.lookup_chain(tok.text)
        if sym is None:
            return True
        if sym.kind == CSharpSymbolKind.TypeParameter:
            return False
        if sym.kind == CSharpSymbolKind.Type and isinstance(sym, CSharpTypeSymbol):
            if sym.type_kind in (CSharpTypeKind.Struct, CSharpTypeKind.Enum):
                return False
        return True

    def IsDelegateTypeName(self):
        tok = self._input.LT(1)
        if tok is None:
            return False
        sym = self.SymTable.current_scope.lookup_chain(tok.text)
        if sym is None or sym.kind != CSharpSymbolKind.Type:
            return False
        return isinstance(sym, CSharpTypeSymbol) and sym.type_kind == CSharpTypeKind.Delegate

    def IsInterfaceTypeName(self):
        tok = self._input.LT(1)
        if tok is None:
            return False
        sym = self.SymTable.current_scope.lookup_chain(tok.text)
        if sym is None or sym.kind != CSharpSymbolKind.Type:
            return False
        return isinstance(sym, CSharpTypeSymbol) and sym.type_kind == CSharpTypeKind.Interface

    def IsClassTypeName(self):
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
        else:
            from CSharpLexer import CSharpLexer
        tok = self._input.LT(1)
        if tok is None:
            return True
        if tok.type in (CSharpLexer.KW_OBJECT, CSharpLexer.KW_STRING):
            return True
        sym = self.SymTable.current_scope.lookup_chain(tok.text)
        if sym is None:
            return True
        if sym.kind != CSharpSymbolKind.Type:
            return True
        if not isinstance(sym, CSharpTypeSymbol):
            return True
        return sym.type_kind not in (CSharpTypeKind.Interface, CSharpTypeKind.Delegate)

    def _classBaseTypeCheck(self, wantInterface):
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
        else:
            from CSharpLexer import CSharpLexer
        tok = self._input.LT(2)
        if tok is None:
            return not wantInterface
        if tok.type in (CSharpLexer.KW_OBJECT, CSharpLexer.KW_STRING):
            return not wantInterface
        sym = self.SymTable.current_scope.lookup_chain(tok.text)
        if sym is None or sym.kind != CSharpSymbolKind.Type:
            return not wantInterface
        if not isinstance(sym, CSharpTypeSymbol):
            return not wantInterface
        is_iface = sym.type_kind == CSharpTypeKind.Interface
        return is_iface if wantInterface else not is_iface

    def IsClassBaseInterfaceList(self):
        return self._classBaseTypeCheck(True)

    def IsClassBaseClassType(self):
        return self._classBaseTypeCheck(False)

    def IsDeclarationPatternAhead(self):
        # Speculative parse: create a new parser on the same stream, try to parse
        # a type(), then check if LT(1) is a simple designation.
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
            from .CSharpParser import CSharpParser
        else:
            from CSharpLexer import CSharpLexer
            from CSharpParser import CSharpParser
        from antlr4 import BailErrorStrategy
        saved_index = self._input.index
        par = CSharpParser(self._input)
        par.removeErrorListeners()
        par._errHandler = BailErrorStrategy()
        try:
            par.type_()
            next_tok = self._input.LT(1)
            return next_tok is not None and (
                next_tok.type == CSharpLexer.Simple_Identifier or next_tok.text == "_"
            )
        except Exception:
            return False
        finally:
            self._input.seek(saved_index)

    def IsConstantPatternAhead(self):
        return not self.IsDeclarationPatternAhead()

    def IsImplicitlyTypedLocalVariable(self):
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
        else:
            from CSharpLexer import CSharpLexer
        tok = self._input.LT(1)
        if tok is None:
            return True
        if tok.type != CSharpLexer.KW_VAR:
            return False
        sym = self.SymTable.current_scope.lookup_chain("var")
        if sym is not None and sym.kind == CSharpSymbolKind.Type:
            return False
        lt3 = self._input.LT(3)
        if lt3 is None or lt3.text != "=":
            return False
        lt4 = self._input.LT(4)
        if lt4 is not None and lt4.text == "{":
            return False
        return True

    def IsExplicitlyTypedLocalVariable(self):
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
        else:
            from CSharpLexer import CSharpLexer
        tok = self._input.LT(1)
        if tok is None:
            return True
        if tok.type != CSharpLexer.KW_VAR:
            return True
        sym = self.SymTable.current_scope.lookup_chain("var")
        if sym is not None and sym.kind == CSharpSymbolKind.Type:
            return True
        lt3 = self._input.LT(3)
        if lt3 is None or lt3.text != "=":
            return True
        lt4 = self._input.LT(4)
        if lt4 is not None and lt4.text == "{":
            return True
        return False

    def IsExplicitlyTypedRefLocalVariable(self):
        if "." in __name__:
            from .CSharpLexer import CSharpLexer
        else:
            from CSharpLexer import CSharpLexer
        tok = self._input.LT(1)
        return tok is not None and tok.type == CSharpLexer.KW_REF

    # Gates comma-separated declarators: False when type is 'var' (only one declarator allowed).
    def IsLocalVariableDeclaration(self):
        if "." in __name__:
            from .CSharpParser import CSharpParser
        else:
            from CSharpParser import CSharpParser
        local_var_decl = self._ctx
        if not isinstance(local_var_decl, CSharpParser.Local_variable_declarationContext):
            return True
        local_variable_type = local_var_decl.local_variable_type()
        if local_variable_type is None:
            return True
        return local_variable_type.getText() != "var"


# =============================================================================
# CSharpSymbolTable — symbol table for parse disambiguation
# =============================================================================

class CSharpSymbolKind:
    Type          = "Type"
    Variable      = "Variable"
    Namespace     = "Namespace"
    TypeParameter = "TypeParameter"
    Alias         = "Alias"

class CSharpTypeKind:
    Class     = "Class"
    Struct    = "Struct"
    Interface = "Interface"
    Enum      = "Enum"
    Delegate  = "Delegate"

class CSharpScopeKind:
    Global    = "Global"
    Namespace = "Namespace"
    Type      = "Type"
    Method    = "Method"
    Block     = "Block"

class CSharpSymbol:
    def __init__(self, name, kind):
        self.name = name
        self.kind = kind

class CSharpTypeSymbol(CSharpSymbol):
    def __init__(self, name, type_kind, arity=0):
        super().__init__(name, CSharpSymbolKind.Type)
        self.type_kind = type_kind
        self.arity = arity

class CSharpVariableSymbol(CSharpSymbol):
    def __init__(self, name, type_ref):
        super().__init__(name, CSharpSymbolKind.Variable)
        self.type_ref = type_ref

class CSharpScope:
    def __init__(self, kind, parent, name=""):
        self.kind   = kind
        self.parent = parent
        self.name   = name
        self._symbols = {}

    def declare(self, sym):
        self._symbols[sym.name] = sym

    def lookup(self, name):
        return self._symbols.get(name, None)

    def lookup_chain(self, name):
        s = self.lookup(name)
        if s is not None:
            return s
        if self.parent is not None:
            return self.parent.lookup_chain(name)
        return None

_CS_RESERVED = {
    "abstract","as","base","bool","break","byte","case","catch","char","checked",
    "class","const","continue","decimal","default","delegate","do","double","else",
    "enum","event","explicit","extern","false","finally","fixed","float","for",
    "foreach","goto","if","implicit","in","int","interface","internal","is","lock",
    "long","namespace","new","null","object","operator","out","override","params",
    "private","protected","public","readonly","ref","return","sbyte","sealed","short",
    "sizeof","stackalloc","static","string","struct","switch","this","throw","true",
    "try","typeof","uint","ulong","unchecked","unsafe","ushort","using","virtual",
    "void","volatile","while",
}

def _cs_is_ident_like(t):
    if not t:
        return False
    c = t[0]
    return c.isalpha() or c == '_' or c == '@'

class CSharpSymbolTable:
    def __init__(self):
        self._known_type_names = set()
        self._generic_arities  = {}
        self._scope_stack      = []
        self.global_scope = CSharpScope(CSharpScopeKind.Global, None, "<global>")
        self._scope_stack.append(self.global_scope)
        self._populate_builtins()

    @property
    def current_scope(self):
        return self._scope_stack[-1]

    def is_type_name(self, name):
        sym = self.current_scope.lookup_chain(name)
        if sym is not None:
            return sym.kind in (CSharpSymbolKind.Type,
                                CSharpSymbolKind.TypeParameter,
                                CSharpSymbolKind.Alias)
        return name in self._known_type_names

    def enter_scope(self, kind, name=""):
        self._scope_stack.append(CSharpScope(kind, self._scope_stack[-1], name))

    def exit_scope(self):
        if len(self._scope_stack) > 1:
            self._scope_stack.pop()

    def declare_type(self, name, kind, arity=0):
        self.current_scope.declare(CSharpTypeSymbol(name, kind, arity))
        self._known_type_names.add(name)
        if arity > 0:
            self._generic_arities.setdefault(name, set()).add(arity)

    def declare_type_param(self, name):
        s = CSharpSymbol(name, CSharpSymbolKind.TypeParameter)
        self.current_scope.declare(s)
        self._known_type_names.add(name)

    def declare_variable(self, name, type_ref):
        self.current_scope.declare(CSharpVariableSymbol(name, type_ref))

    def declare_alias(self, alias, target):
        s = CSharpSymbol(alias, CSharpSymbolKind.Alias)
        self.current_scope.declare(s)
        self._known_type_names.add(alias)

    def import_namespace(self, ns):
        s = CSharpSymbol(ns, CSharpSymbolKind.Namespace)
        self.current_scope.declare(s)

    def pre_scan(self, tokens):
        try:
            tokens.fill()
        except Exception:
            pass
        n = tokens.size
        for i in range(n):
            tok = tokens.get(i)
            if tok.channel != 0:
                continue
            txt = tok.text
            if txt in ("class", "struct", "interface", "enum"):
                j = self._next_default(tokens, i + 1, n)
                while j < n and tokens.get(j).text in ("partial", "ref"):
                    j = self._next_default(tokens, j + 1, n)
                if j < n and _cs_is_ident_like(tokens.get(j).text):
                    type_name = tokens.get(j).text
                    arity, type_params = self._count_type_params(tokens, j + 1, n)
                    self._register_type(type_name, arity, type_params)
            elif txt == "delegate":
                j = self._next_default(tokens, i + 1, n)
                while j < n and tokens.get(j).text in ("ref", "readonly"):
                    j = self._next_default(tokens, j + 1, n)
                if j >= n:
                    continue
                j = self._next_default(tokens, j + 1, n)
                if j < n and tokens.get(j).text == "<":
                    j = self._skip_angled(tokens, j, n)
                while j < n and tokens.get(j).text in ("[", "]", ","):
                    j = self._next_default(tokens, j + 1, n)
                if j < n and tokens.get(j).text == "?":
                    j = self._next_default(tokens, j + 1, n)
                if j < n and _cs_is_ident_like(tokens.get(j).text):
                    type_name = tokens.get(j).text
                    arity, type_params = self._count_type_params(tokens, j + 1, n)
                    self._register_type(type_name, arity, type_params)

    def _register_type(self, name, arity, type_params):
        if name not in _CS_RESERVED:
            self._known_type_names.add(name)
        if arity > 0 and name not in _CS_RESERVED:
            self._generic_arities.setdefault(name, set()).add(arity)
        for tp in type_params:
            if _cs_is_ident_like(tp) and tp not in _CS_RESERVED:
                self._known_type_names.add(tp)

    def _next_default(self, tokens, pos, n):
        while pos < n and tokens.get(pos).channel != 0:
            pos += 1
        return pos

    def _skip_angled(self, tokens, pos, n):
        depth = 1
        i = self._next_default(tokens, pos + 1, n)
        while i < n and depth > 0:
            t = tokens.get(i).text
            if t == "<":
                depth += 1
            elif t == ">":
                depth -= 1
            i = self._next_default(tokens, i + 1, n)
        return i

    def _count_type_params(self, tokens, pos, n):
        p = self._next_default(tokens, pos, n)
        if p >= n or tokens.get(p).text != "<":
            return 0, []
        depth = 1
        arity = 1
        names = []
        i = self._next_default(tokens, p + 1, n)
        while i < n and depth > 0:
            t = tokens.get(i).text
            if t == "<":
                depth += 1
            elif t == ">":
                depth -= 1
            elif t == "," and depth == 1:
                arity += 1
            elif depth == 1 and _cs_is_ident_like(t):
                names.append(t)
            i = self._next_default(tokens, i + 1, n)
        if depth == 0:
            return arity, names
        return 0, []

    def _populate_builtins(self):
        for t in (
            "bool","byte","sbyte","char","decimal","double","float","int","uint",
            "long","ulong","short","ushort","object","string","void","dynamic",
            "Boolean","Byte","SByte","Char","Decimal","Double","Single",
            "Int16","Int32","Int64","UInt16","UInt32","UInt64",
            "IntPtr","UIntPtr","Object","String","Void",
            "Guid","DateTime","DateTimeOffset","TimeSpan","Uri",
            "Exception","Type","Enum","Delegate","MulticastDelegate",
            "Attribute","Math","Convert","Console","Environment",
            "List","IList","IEnumerable","IEnumerator","ICollection",
            "IReadOnlyList","IReadOnlyCollection","IReadOnlyDictionary",
            "Dictionary","IDictionary","SortedDictionary","SortedList",
            "HashSet","SortedSet","Queue","Stack","LinkedList","LinkedListNode",
            "Func","Action","Predicate","Comparison","Converter",
            "Task","ValueTask","Nullable",
            "IAsyncEnumerable","IAsyncEnumerator",
            "Span","ReadOnlySpan","Memory","ReadOnlyMemory",
            "KeyValuePair","Tuple",
            "ImmutableArray","ImmutableList","ImmutableDictionary","ImmutableHashSet",
            "Lazy","WeakReference","EventHandler",
            "IEqualityComparer","IComparer","EqualityComparer","Comparer",
            "ConcurrentDictionary","ConcurrentQueue","ConcurrentStack","ConcurrentBag",
        ):
            self._known_type_names.add(t)
