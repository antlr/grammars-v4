import sys
import os
from antlr4 import Parser, ParserRuleContext


class CSharpParserBase(Parser):

    def __init__(self, input, output=sys.stdout):
        super().__init__(input, output)

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
        for listener in self._parseListeners if self._parseListeners else []:
            pass
        self.notifyErrorListeners(msg)

    # -------------------------------------------------------------------------
    # ReduceTree — post-parse cleanup (for external tooling)
    # -------------------------------------------------------------------------
    def ReduceTree(self, currentctx):
        if "." in __name__:
            from .CSharpParser import CSharpParser
        else:
            from CSharpParser import CSharpParser

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
            from .CSharpParser import CSharpParser
        else:
            from CSharpLexer import CSharpLexer
            from CSharpParser import CSharpParser

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
        tokenForError = childRule.getChild(1)
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
            return  # initializer present — check passes

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
