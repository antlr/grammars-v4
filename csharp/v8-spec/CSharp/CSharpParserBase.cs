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
        var t = ((CommonTokenStream)this.InputStream).LT(1);
        if (t == null) return true;
        var n = t.Text;
        if (t.Type != CSharpLexer.KW_VAR)
            return true;
        if (this.SymTable.CurrentScope.LookupChain("var") is CSharpSymbol sym
            && sym.Kind == CSharpSymbolKind.Type)
		return true;
	var p = ((CommonTokenStream)this.InputStream).LT(1);
	if (p == null) return true;
	if (p.Type == CSharpLexer.KW_VAR)
		return true;
        return false;
    }

    public bool IsImplicitlyTypedLocalVariable()
    {
        var t = ((CommonTokenStream)this.InputStream).LT(1);
        if (t == null) return true;
        var n = t.Text;
        if (t.Type != CSharpLexer.KW_VAR)
            return false;
        if (this.SymTable.CurrentScope.LookupChain("var") is CSharpSymbol sym
            && sym.Kind == CSharpSymbolKind.Type)
            return false;
        return true;
    }
}

