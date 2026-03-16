import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.List;

public abstract class CSharpParserBase extends Parser
{
    protected CSharpParserBase(TokenStream input)
    {
        super(input);
    }

    // -------------------------------------------------------------------------
    // Look-ahead helpers
    // -------------------------------------------------------------------------

    public boolean LookAheadIs(int pos, int value)
    {
        return _input.LA(pos) == value;
    }

    public boolean LookAheadIsNot(int pos, int value)
    {
        return _input.LA(pos) != value;
    }

    // -------------------------------------------------------------------------
    // Error reporting
    // -------------------------------------------------------------------------

    private void notifySemanticError(int line, int charPositionInLine, String msg)
    {
        notifyErrorListeners(msg);
    }

    // -------------------------------------------------------------------------
    // ReduceTree — post-parse cleanup (for external tooling)
    // -------------------------------------------------------------------------

    public void ReduceTree(ParserRuleContext currentctx)
    {
        String[] names = getRuleNames();
        int ruleTypeArgumentList = -1;
        for (int i = 0; i < names.length; i++)
        {
            if ("type_argument_list".equals(names[i]))
            {
                ruleTypeArgumentList = i;
                break;
            }
        }
        final int tal = ruleTypeArgumentList;
        takeOutEmpties(currentctx, tal);

        String reduceProperty = System.getenv("ANTLR_REDUCE_TREE");
        if (!"yes".equalsIgnoreCase(reduceProperty)) return;

        boolean reduceAllChildren = "yes".equalsIgnoreCase(System.getenv("ANTLR_REDUCE_ALL_CHILDREN"));
        reducer(currentctx, reduceAllChildren);
    }

    private static void takeOutEmpties(ParserRuleContext node, int tal)
    {
        if (node == null || node.children == null) return;
        for (int ix = node.children.size() - 1; ix >= 0; ix--)
        {
            ParseTree child = node.children.get(ix);
            if (child instanceof ParserRuleContext)
            {
                ParserRuleContext childCtx = (ParserRuleContext) child;
                if (childCtx.getRuleIndex() == tal && childCtx.getChildCount() == 0)
                    node.children.remove(ix);
                else
                    takeOutEmpties(childCtx, tal);
            }
        }
    }

    private static void reducer(ParserRuleContext node, boolean reduceAllChildren)
    {
        if (node == null || node.children == null) return;
        for (ParseTree child : node.children)
            if (child instanceof ParserRuleContext) reducer((ParserRuleContext) child, reduceAllChildren);
        if (reduceAllChildren || node.getChildCount() == 1)
        {
            for (int i = 0; i < node.children.size(); i++)
            {
                ParseTree child = node.children.get(i);
                if (child instanceof ParserRuleContext)
                {
                    ParserRuleContext childNode = (ParserRuleContext) child;
                    if (childNode.getChildCount() == 1)
                    {
                        ParseTree grandchild = childNode.getChild(0);
                        if (grandchild instanceof ParserRuleContext)
                            node.children.set(i, grandchild);
                    }
                }
            }
        }
    }

    // -------------------------------------------------------------------------
    // insertNode — rewire parse tree (MLR un-inlining)
    // -------------------------------------------------------------------------

    private void insertNode(ParserRuleContext currentctx, String contextTypeName)
    {
        try
        {
            Class<?> contextType = null;
            for (Class<?> inner : getClass().getDeclaredClasses())
            {
                if (inner.getSimpleName().equals(contextTypeName))
                {
                    contextType = inner;
                    break;
                }
            }
            if (contextType == null) return;

            Constructor<?> ctor = contextType.getConstructor(ParserRuleContext.class, int.class);
            ParserRuleContext insertedctx = (ParserRuleContext) ctor.newInstance(
                currentctx, currentctx.invokingState);

            insertedctx.children = (currentctx.children != null)
                ? new ArrayList<>(currentctx.children)
                : new ArrayList<>();
            if (currentctx.children == null)
                currentctx.children = new ArrayList<>();
            currentctx.children.clear();
            currentctx.children.add(insertedctx);
        }
        catch (Exception e)
        {
            throw new RuntimeException("insertNode failed: " + e.getMessage(), e);
        }
    }

    public void AsInvocationExpression(ParserRuleContext currentctx)         { insertNode(currentctx, "Invocation_expressionContext"); }
    public void AsElementAccess(ParserRuleContext currentctx)                { insertNode(currentctx, "Element_accessContext"); }
    public void AsMemberAccess(ParserRuleContext currentctx)                 { insertNode(currentctx, "Member_accessContext"); }
    public void AsNullConditionalMemberAccess(ParserRuleContext currentctx)  { insertNode(currentctx, "Null_conditional_member_accessContext"); }
    public void AsNullConditionalElementAccess(ParserRuleContext currentctx) { insertNode(currentctx, "Null_conditional_element_accessContext"); }
    public void AsPostIncrementExpression(ParserRuleContext currentctx)      { insertNode(currentctx, "Post_increment_expressionContext"); }
    public void AsPostDecrementExpression(ParserRuleContext currentctx)      { insertNode(currentctx, "Post_decrement_expressionContext"); }
    public void AsNullForgivingExpression(ParserRuleContext currentctx)      { insertNode(currentctx, "Null_forgiving_expressionContext"); }
    public void AsPointerMemberAccess(ParserRuleContext currentctx)          { insertNode(currentctx, "Pointer_member_accessContext"); }

    // -------------------------------------------------------------------------
    // ElementAccessSemanticCheck
    // -------------------------------------------------------------------------

    private static int ruleIdx(String[] names, String name)
    {
        for (int i = 0; i < names.length; i++) if (name.equals(names[i])) return i;
        return -1;
    }

    public void ElementAccessSemanticCheck(ParserRuleContext currentctx)
    {
        String[] names = getRuleNames();

        int rulePrimaryExpression            = ruleIdx(names, "primary_expression");
        int ruleElementAccess                = ruleIdx(names, "element_access");
        int rulePointerElementAccess         = ruleIdx(names, "pointer_element_access");
        int ruleNullConditionalElementAccess = ruleIdx(names, "null_conditional_element_access");
        int ruleArrayCreationExpression      = ruleIdx(names, "array_creation_expression");
        int ruleStackallocExpression         = ruleIdx(names, "stackalloc_expression");

        if (currentctx.getRuleIndex() != rulePrimaryExpression || currentctx.getChildCount() != 1)
            return;

        ParseTree childTree = currentctx.getChild(0);
        if (!(childTree instanceof ParserRuleContext)) return;
        ParserRuleContext childRule = (ParserRuleContext) childTree;

        int childRuleIndex  = childRule.getRuleIndex();
        int childChildCount = childRule.getChildCount();

        if (childRuleIndex == ruleElementAccess || childRuleIndex == rulePointerElementAccess)
        {
            if (childChildCount != 4) return;
        }
        else if (childRuleIndex == ruleNullConditionalElementAccess)
        {
            if (childChildCount != 5) return;
        }
        else return;

        ParseTree accessTargetTree = childRule.getChild(0);
        if (!(accessTargetTree instanceof ParserRuleContext)) return;
        ParserRuleContext accessTarget = (ParserRuleContext) accessTargetTree;

        if (accessTarget.getRuleIndex() != rulePrimaryExpression || accessTarget.getChildCount() == 0)
            return;

        ParseTree lhsTargetTree = accessTarget.getChild(0);
        if (!(lhsTargetTree instanceof ParserRuleContext)) return;
        ParserRuleContext lhsTarget = (ParserRuleContext) lhsTargetTree;

        if (lhsTarget.getRuleIndex() != ruleArrayCreationExpression
            && lhsTarget.getRuleIndex() != ruleStackallocExpression)
            return;

        Token lhsLast     = lhsTarget.getStop();
        int   lhsLastType = lhsLast != null ? lhsLast.getType() : -1;

        if (lhsLastType == CSharpLexer.TK_RBRACE) return; // initializer present

        if (lhsLastType != CSharpLexer.TK_RBRACK)
        {
            System.err.printf("%d:%d Error: Unexpected LHS last token %s (%d).%n",
                lhsLast.getLine(), lhsLast.getCharPositionInLine(),
                lhsLast.getText(), lhsLastType);
            return;
        }

        String childRuleName = names[childRuleIndex];
        String lhsRuleName   = names[lhsTarget.getRuleIndex()];
        String childPrefix   = "AEIOUaeiou".indexOf(childRuleName.charAt(0)) >= 0 ? "an" : "a";
        String lhsPrefix     = "AEIOUaeiou".indexOf(lhsRuleName.charAt(0))   >= 0 ? "an" : "a";

        notifySemanticError(lhsLast.getLine(), lhsLast.getCharPositionInLine(),
            "LHS of " + childPrefix + " " + childRuleName +
            " cannot be " + lhsPrefix + " " + lhsRuleName + " unless it has an initializer");
    }
}
