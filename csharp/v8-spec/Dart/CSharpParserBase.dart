import 'dart:io';
import 'package:antlr4/antlr4.dart';
import 'CSharpLexer.dart';

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
}
