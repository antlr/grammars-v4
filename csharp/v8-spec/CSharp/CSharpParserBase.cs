using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;

public abstract class CSharpParserBase : Parser
{
    protected CSharpParserBase(ITokenStream input) : base(input) { }
    protected CSharpParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) { }

    // Rule-index lookup by name (RULE_* constants live on the generated subclass).
    private int RuleIdx(string ruleName) => Array.IndexOf(RuleNames, ruleName);

    //======================================================================================
    // Symbol table for parse disambiguation (cast vs. parenthesised, etc.)

    public CSharpSymbolTable SymTable { get; } = new CSharpSymbolTable();

    //--------------------------------------------------------------------------------------
    // Semantic predicate — cast_expression
    //
    // Fires at the start of the cast_expression alternative inside unary_expression.
    // At that moment LT(1) = '(' so LT(2) is the first token inside the parens.
    //
    // Strategy: only BLOCK the cast alternative when the first token inside the
    // parens is definitively a variable (not a type).  For unknown names and
    // qualified names (e.g. System.String) we return true and let ANTLR's LL(*)
    // lookahead continue to resolve the ambiguity.  This avoids false negatives
    // on forward-declared or externally-defined types.
    public bool IsCastExpressionAhead()
    {
        IToken tok = ((ITokenStream)InputStream).LT(2);
        if (tok == null) return true;
        CSharpSymbol sym = SymTable.CurrentScope.LookupChain(tok.Text);
        // Return false only when the name is explicitly declared as a variable.
        return sym == null || sym.Kind != CSharpSymbolKind.Variable;
    }

    //--------------------------------------------------------------------------------------
    // Scope management — called from grammar actions; no parameters required.

    public void EnterNamespaceScope() => SymTable.EnterScope(CSharpScopeKind.Namespace);
    public void EnterTypeScope()      => SymTable.EnterScope(CSharpScopeKind.Type);
    public void EnterBlockScope()     => SymTable.EnterScope(CSharpScopeKind.Block);
    public void ExitCurrentScope()    => SymTable.ExitScope();

    //--------------------------------------------------------------------------------------
    // Type-parameter declaration — fires after the identifier token is consumed.
    //
    // Context is the type_parameter rule context whose single child is the
    // identifier that was just matched.
    public void OnTypeParameter()
    {
        string name = Context.GetChild(Context.ChildCount - 1).GetText();
        SymTable.DeclareTypeParam(name);
    }

    //--------------------------------------------------------------------------------------
    // Using-directive handlers — fire at the end of the respective rules.

    // using_alias_directive : 'using' identifier '=' namespace_or_type_name ';'
    //   child[0]='using'  child[1]=identifier  child[2]='='
    //   child[3]=namespace_or_type_name  child[4]=';'
    public void OnUsingAliasDirective()
    {
        string alias  = Context.GetChild(1).GetText();
        string target = Context.GetChild(3).GetText();
        SymTable.DeclareAlias(alias, target);
    }

    // using_namespace_directive : 'using' namespace_name ';'
    //   child[0]='using'  child[1]=namespace_name  child[2]=';'
    public void OnUsingNamespaceDirective()
    {
        string ns = Context.GetChild(1).GetText();
        SymTable.ImportNamespace(ns);
    }

    //--------------------------------------------------------------------------------------
    // Variable declaration helpers — called in two steps:
    //   1. BeginVariableDeclaration() fires after 'type' is consumed; the type rule
    //      context is the last child added to the enclosing declaration context so far.
    //   2. OnVariableDeclarator() fires after each 'identifier' declarator is consumed.

    private string _pendingVarType = "?";

    public void BeginVariableDeclaration()
    {
        // The 'type' rule is the last child matched in the current rule context so far.
        _pendingVarType = Context.GetChild(Context.ChildCount - 1).GetText();
    }

    // Shared by variable_declarator (fields) and explicitly_typed_local_variable_declarator.
    // Fires after 'identifier' is consumed; it is the only child present at that point.
    public void OnVariableDeclarator()
    {
        string id = Context.GetChild(Context.ChildCount - 1).GetText();
        SymTable.DeclareVariable(id, _pendingVarType);
    }

    //======================================================================================

    private void NotifySemanticError(int line, int charPositionInLine, string msg)
    {
        IAntlrErrorListener<IToken> listener = ErrorListenerDispatch;
        listener.SyntaxError(Console.Error, this, null, line, charPositionInLine, msg, null);
    }

    //======================================================================================

    // Convenience predicates — names must match Macros.json expansions.
    public bool LookAheadIs(int pos, int value)    => InputStream.LA(pos) == value;
    public bool LookAheadIsNot(int pos, int value) => InputStream.LA(pos) != value;

    //======================================================================================

    // Post-parse tree cleanup (retained for external tooling; not called from grammar).
    //  Pass 1: Remove empty type_argument_list nodes — artifacts of the '<'
    //          disambiguation predicate.
    //  Pass 2: Optionally collapse single-parent/child chains (off by default;
    //          set env var ANTLR_REDUCE_TREE=yes to enable).

    public void ReduceTree(ParserRuleContext currentctx)
    {
        int ruleTypeArgumentList = RuleIdx("type_argument_list");

        void takeOutEmpties(IParseTree node)
        {
            if (node is ParserRuleContext parent && parent.children != null)
            {
                for (int ix = parent.children.Count - 1; ix >= 0; ix--)
                {
                    var anyTarget = parent.children[ix];
                    if (anyTarget is ParserRuleContext childTarget)
                    {
                        if (childTarget.RuleIndex == ruleTypeArgumentList
                            && childTarget.ChildCount == 0)
                            parent.children.RemoveAt(ix);
                        else
                            takeOutEmpties(childTarget);
                    }
                }
            }
        }
        takeOutEmpties(currentctx);

        string reduceProperty = (Environment.GetEnvironmentVariable("ANTLR_REDUCE_TREE") ?? "no").ToLowerInvariant();
        if (reduceProperty != "yes") return;

        string reduceAllChildrenProperty = (Environment.GetEnvironmentVariable("ANTLR_REDUCE_ALL_CHILDREN") ?? "no").ToLowerInvariant();
        bool reduceAllChildren = reduceAllChildrenProperty == "yes";

        void reducer(IParseTree node)
        {
            if (node is ParserRuleContext parent && parent.children != null)
            {
                foreach (var child in parent.children)
                    reducer(child);
                if (reduceAllChildren || parent.ChildCount == 1)
                {
                    for (int i = 0; i < parent.children.Count; i++)
                    {
                        var child = parent.children[i];
                        if (child is ParserRuleContext childNode && childNode.ChildCount == 1)
                        {
                            var grandchild = childNode.GetChild(0);
                            if (grandchild is ParserRuleContext grandchildNode)
                                parent.children[i] = grandchildNode;
                        }
                    }
                }
            }
        }
        if (currentctx.children != null)
            reducer(currentctx);
    }

    //======================================================================================

    // Helpers to "undo" MLR inlining in primary_expression:
    // Moves all current children of currentctx into a new child context of the
    // appropriate type, restoring parse tree nodes elided by MLR removal.
    //
    // The context types (e.g. Invocation_expressionContext) are nested classes of the
    // generated CSharpParser subclass.  We create them via reflection so that this base
    // class does not need a compile-time reference to the generated class.

    private void InsertNode(ParserRuleContext currentctx, string contextTypeName)
    {
return;
        Type parserType  = typeof(CSharpParser);
        Type contextType = parserType.GetNestedType(contextTypeName)
            ?? throw new InvalidOperationException(
                $"Nested type '{contextTypeName}' not found on {parserType.Name}.");

        var insertedctx = (ParserRuleContext)Activator.CreateInstance(
            contextType, currentctx, currentctx.invokingState);

        insertedctx.children = currentctx.children != null
            ? new List<IParseTree>(currentctx.children)
            : new List<IParseTree>();
        if (currentctx.children == null)
            currentctx.children = new List<IParseTree>();
        currentctx.children.Clear();
        currentctx.children.Add(insertedctx);
    }

    public void AsInvocationExpression(ParserRuleContext currentctx)
        => InsertNode(currentctx, "Invocation_expressionContext");

    public void AsElementAccess(ParserRuleContext currentctx)
        => InsertNode(currentctx, "Element_accessContext");

    public void AsMemberAccess(ParserRuleContext currentctx)
        => InsertNode(currentctx, "Member_accessContext");

    public void AsNullConditionalMemberAccess(ParserRuleContext currentctx)
        => InsertNode(currentctx, "Null_conditional_member_accessContext");

    public void AsNullConditionalElementAccess(ParserRuleContext currentctx)
        => InsertNode(currentctx, "Null_conditional_element_accessContext");

    public void AsPostIncrementExpression(ParserRuleContext currentctx)
        => InsertNode(currentctx, "Post_increment_expressionContext");

    public void AsPostDecrementExpression(ParserRuleContext currentctx)
        => InsertNode(currentctx, "Post_decrement_expressionContext");

    public void AsNullForgivingExpression(ParserRuleContext currentctx)
        => InsertNode(currentctx, "Null_forgiving_expressionContext");

    public void AsPointerMemberAccess(ParserRuleContext currentctx)
        => InsertNode(currentctx, "Pointer_member_accessContext");

    //======================================================================================

    // Semantic check from §12.8.11-2:
    // array_creation_expression[...] and stackalloc_expression[...] must have
    // an initializer on the LHS of an element access.

    public void ElementAccessSemanticCheck(ParserRuleContext currentctx)
    {
        int rulePrimaryExpression            = RuleIdx("primary_expression");
        int ruleElementAccess                = RuleIdx("element_access");
        int rulePointerElementAccess         = RuleIdx("pointer_element_access");
        int ruleNullConditionalElementAccess = RuleIdx("null_conditional_element_access");
        int ruleArrayCreationExpression      = RuleIdx("array_creation_expression");
        int ruleStackallocExpression         = RuleIdx("stackalloc_expression");

        if (currentctx.RuleIndex != rulePrimaryExpression || currentctx.ChildCount != 1)
            return;

        var childTree = currentctx.GetChild(0);
        if (childTree is not ParserRuleContext childRule) return;

        int childRuleIndex  = childRule.RuleIndex;
        int childChildCount = childRule.ChildCount;

        if (childRuleIndex == ruleElementAccess || childRuleIndex == rulePointerElementAccess)
        {
            if (childChildCount != 4) return;
        }
        else if (childRuleIndex == ruleNullConditionalElementAccess)
        {
            if (childChildCount != 5) return;
        }
        else
        {
            return;
        }

        var accessTargetTree      = childRule.GetChild(0);
        var tokenForErrorLocation = childRule.GetChild(1);

        if (accessTargetTree is not ParserRuleContext accessTarget) return;
        if (accessTarget.RuleIndex != rulePrimaryExpression || accessTarget.ChildCount == 0) return;

        var lhsTargetTree = accessTarget.GetChild(0);
        if (lhsTargetTree is not ParserRuleContext lhsTarget) return;

        if (lhsTarget.RuleIndex != ruleArrayCreationExpression
            && lhsTarget.RuleIndex != ruleStackallocExpression)
            return;

        IToken lhsLast     = lhsTarget.Stop;
        int    lhsLastType = lhsLast.Type;
        string lhsLastText = lhsLast.Text;

        if (lhsLastType == CSharpLexer.TK_RBRACE) return; // initializer present — check passes

        if (lhsLastType != CSharpLexer.TK_RBRACK)
        {
            Console.Error.WriteLine(
                $"{lhsLast.Line}:{lhsLast.Column} Error: Unexpected LHS last token {lhsLastText} ({lhsLastType}).");
            return;
        }

        IToken reportAt      = tokenForErrorLocation is TerminalNodeImpl tni ? tni.Symbol : lhsLast;
        string childRuleName = RuleNames[childRuleIndex];
        string lhsRuleName   = RuleNames[lhsTarget.RuleIndex];
        string childPrefix   = "AEIOUaeiou".IndexOf(childRuleName[0]) != -1 ? "an" : "a";
        string lhsPrefix     = "AEIOUaeiou".IndexOf(lhsRuleName[0])   != -1 ? "an" : "a";

        NotifySemanticError(reportAt.Line, reportAt.Column,
            $"LHS of {childPrefix} {childRuleName} cannot be {lhsPrefix} {lhsRuleName} unless it has an initializer");
    }

    public bool IsExplicitlyTypedLocalVariable()
    {
        IToken t = ((CommonTokenStream)InputStream).LT(1);
        if (t == null) return true;
        if (t.Type != CSharpLexer.KW_VAR) return true;  // not 'var' → always explicit
        if (SymTable.CurrentScope.LookupChain("var") is CSharpSymbol sym
            && sym.Kind == CSharpSymbolKind.Type) return true;  // 'var' is a type name
        // LT(1)='var' LT(2)=identifier LT(3)='=' or something else
        IToken lt3 = ((CommonTokenStream)InputStream).LT(3);
        if (lt3 == null || lt3.Text != "=") return true;   // no '=' → explicit
        IToken lt4 = ((CommonTokenStream)InputStream).LT(4);
        if (lt4 != null && lt4.Text == "{") return true;   // array_initializer → explicit
//	System.Console.WriteLine("t1f = " + t);
        return false;
    }

    public bool IsExplicitlyTypedRefLocalVariable()
    {
        IToken t = ((CommonTokenStream)InputStream).LT(1);
        return t != null && t.Type == CSharpLexer.KW_REF;
    }

    public bool IsImplicitlyTypedLocalVariable()
    {
        IToken t = ((CommonTokenStream)InputStream).LT(1);
        if (t == null) return true;
        if (t.Type != CSharpLexer.KW_VAR) return false;
        if (SymTable.CurrentScope.LookupChain("var") is CSharpSymbol sym
            && sym.Kind == CSharpSymbolKind.Type) return false;
        IToken lt3 = ((CommonTokenStream)InputStream).LT(3);
        if (lt3 == null || lt3.Text != "=") return false;  // no '=' → not implicitly typed
        IToken lt4 = ((CommonTokenStream)InputStream).LT(4);
        if (lt4 != null && lt4.Text == "{") return false;  // array_initializer → not implicitly typed
//	System.Console.WriteLine("t2t = " + t);
        return true;
    }

    //--------------------------------------------------------------------------------------
    // type rule disambiguation — decisions 13 / 15
    //
    // The type rule has four alternatives:
    //   reference_type | value_type | type_parameter | pointer_type
    // For a bare identifier all three non-pointer alts are viable, so we add
    // mutually-exclusive predicates to the first three alternatives.

    // True only when LT(1) is declared as a type parameter in the current scope chain.
    public bool IsTypeParameterName()
    {
        IToken t = ((CommonTokenStream)InputStream).LT(1);
        if (t == null) return false;
        CSharpSymbol sym = SymTable.CurrentScope.LookupChain(t.Text);
        return sym != null && sym.Kind == CSharpSymbolKind.TypeParameter;
    }

    // True when LT(1) is unambiguously a value type: a simple-type keyword,
    // a tuple opener '(', or an identifier declared as a struct/enum.
    public bool IsValueTypeName()
    {
        IToken t = ((CommonTokenStream)InputStream).LT(1);
        if (t == null) return false;
        switch (t.Type)
        {
            case CSharpLexer.KW_BOOL:
            case CSharpLexer.KW_BYTE:
            case CSharpLexer.KW_CHAR:
            case CSharpLexer.KW_DECIMAL:
            case CSharpLexer.KW_DOUBLE:
            case CSharpLexer.KW_FLOAT:
            case CSharpLexer.KW_INT:
            case CSharpLexer.KW_LONG:
            case CSharpLexer.KW_SBYTE:
            case CSharpLexer.KW_SHORT:
            case CSharpLexer.KW_UINT:
            case CSharpLexer.KW_ULONG:
            case CSharpLexer.KW_USHORT:
            case CSharpLexer.TK_LPAREN:   // tuple_type
                return true;
        }
        CSharpSymbol sym = SymTable.CurrentScope.LookupChain(t.Text);
        if (sym == null || sym.Kind != CSharpSymbolKind.Type) return false;
        CSharpTypeSymbol ts = sym as CSharpTypeSymbol;
        return ts != null && (ts.TypeKind == CSharpTypeKind.Struct || ts.TypeKind == CSharpTypeKind.Enum);
    }

    //--------------------------------------------------------------------------------------
    // pattern rule disambiguation — decision 31
    //
    // declaration_pattern (type simple_designation) and constant_pattern share the same
    // token prefix for inputs like "double d" or "Shape x".  We disambiguate by
    // speculatively parsing a type() — analogous to an ANTLR3 syntactic predicate —
    // then checking whether a simple designation follows.
    //
    // BailErrorStrategy causes the speculative parse to throw on any error rather than
    // attempting recovery, so the token stream remains clean for the rewind.

    public bool IsDeclarationPatternAhead()
    {
        var par = new CSharpParser((ITokenStream)InputStream);
        par.RemoveErrorListeners();
        par.ErrorHandler = new BailErrorStrategy();
        int savedIndex = InputStream.Index;
        try
        {
            var type = par.type_();
            IToken next = ((CommonTokenStream)InputStream).LT(1);
            return next != null
                && (next.Type == CSharpLexer.Simple_Identifier || next.Text == "_");
        }
        catch
        {
            return false;
        }
        finally
        {
            InputStream.Seek(savedIndex);
        }
    }

    public bool IsConstantPatternAhead() => !IsDeclarationPatternAhead();

    //--------------------------------------------------------------------------------------
    // non_nullable_reference_type disambiguation — decision 15
    //
    // class_type, interface_type, and delegate_type all reduce through type_name
    // for a bare identifier, so ANTLR cannot distinguish them by lookahead alone.
    // IsDelegateTypeName and IsInterfaceTypeName fire only on positively-known symbols;
    // IsClassTypeName is the complement/default.

    public bool IsDelegateTypeName()
    {
        IToken t = ((CommonTokenStream)InputStream).LT(1);
        if (t == null) return false;
        CSharpSymbol sym = SymTable.CurrentScope.LookupChain(t.Text);
        if (sym == null || sym.Kind != CSharpSymbolKind.Type) return false;
        CSharpTypeSymbol ts = sym as CSharpTypeSymbol;
        return ts != null && ts.TypeKind == CSharpTypeKind.Delegate;
    }

    public bool IsInterfaceTypeName()
    {
        IToken t = ((CommonTokenStream)InputStream).LT(1);
        if (t == null) return false;
        CSharpSymbol sym = SymTable.CurrentScope.LookupChain(t.Text);
        if (sym == null || sym.Kind != CSharpSymbolKind.Type) return false;
        CSharpTypeSymbol ts = sym as CSharpTypeSymbol;
        return ts != null && ts.TypeKind == CSharpTypeKind.Interface;
    }

    public bool IsClassTypeName()
    {
        IToken t = ((CommonTokenStream)InputStream).LT(1);
        if (t == null) return true;
        switch (t.Type)
        {
            case CSharpLexer.KW_OBJECT:
            case CSharpLexer.KW_STRING:
                return true;  // always class types
        }
        CSharpSymbol sym = SymTable.CurrentScope.LookupChain(t.Text);
        if (sym == null) return true;  // unknown name — default to class_type
        if (sym.Kind != CSharpSymbolKind.Type) return true;
        CSharpTypeSymbol ts = sym as CSharpTypeSymbol;
        if (ts == null) return true;
        return ts.TypeKind != CSharpTypeKind.Interface
            && ts.TypeKind != CSharpTypeKind.Delegate;
    }

    //--------------------------------------------------------------------------------------
    // class_base disambiguation
    //
    // class_base starts with ':' in all alternatives, so the decision point is at LT(1)=':'.
    // Predicates must be at the START of alternatives (before ':') so ANTLR4 evaluates them
    // during prediction — they check LT(2), which is the base type name after the colon.

    private bool ClassBaseTypeCheck(bool wantInterface)
    {
        IToken t = ((CommonTokenStream)InputStream).LT(2);  // LT(1)=':' LT(2)=type name
        if (t == null) return !wantInterface;
        switch (t.Type)
        {
            case CSharpLexer.KW_OBJECT:
            case CSharpLexer.KW_STRING:
                return !wantInterface;  // always class types
        }
        CSharpSymbol sym = SymTable.CurrentScope.LookupChain(t.Text);
        if (sym == null) return !wantInterface;  // unknown → default to class
        if (sym.Kind != CSharpSymbolKind.Type) return !wantInterface;
        CSharpTypeSymbol ts = sym as CSharpTypeSymbol;
        if (ts == null) return !wantInterface;
        bool isInterface = ts.TypeKind == CSharpTypeKind.Interface;
        return wantInterface ? isInterface : !isInterface;
    }

    public bool IsClassBaseInterfaceList() => ClassBaseTypeCheck(true);
    public bool IsClassBaseClassType()     => ClassBaseTypeCheck(false);

    // True when LT(1) is NOT a known type parameter and NOT a known value type.
    // Unambiguous reference-type openers (dynamic, object, string, '[') always return true.
    // Unknown identifiers default to true (open-world assumption).
    public bool IsReferenceTypeName()
    {
        IToken t = ((CommonTokenStream)InputStream).LT(1);
        if (t == null) return true;
        switch (t.Type)
        {
            case CSharpLexer.KW_DYNAMIC:
            case CSharpLexer.KW_OBJECT:
            case CSharpLexer.KW_STRING:
            case CSharpLexer.TK_LBRACK:   // array_type
                return true;
        }
        CSharpSymbol sym = SymTable.CurrentScope.LookupChain(t.Text);
        if (sym == null) return true;  // unknown name — default to reference_type
        if (sym.Kind == CSharpSymbolKind.TypeParameter) return false;
        if (sym.Kind == CSharpSymbolKind.Type)
        {
            CSharpTypeSymbol ts = sym as CSharpTypeSymbol;
            if (ts != null && (ts.TypeKind == CSharpTypeKind.Struct || ts.TypeKind == CSharpTypeKind.Enum))
                return false;
        }
        return true;
    }

    // Used to gate comma-separated declarators in local_variable_declaration.
    // Returns false when the type is 'var' (implicit typing allows only one declarator).
    public bool IsLocalVariableDeclaration()
    {
        var local_var_decl = this.Context as CSharpParser.Local_variable_declarationContext;
        if (local_var_decl == null) return true;
        var local_variable_type = local_var_decl.local_variable_type();
        if (local_variable_type == null) return true;
        return local_variable_type.GetText() != "var";
    }
}

