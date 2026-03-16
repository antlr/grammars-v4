package parser

import (
	"fmt"
	"os"
	"strings"

	"github.com/antlr4-go/antlr/v4"
)

type CSharpParserBase struct {
	*antlr.BaseParser
}

// -------------------------------------------------------------------------
// Look-ahead helpers
// -------------------------------------------------------------------------

func (p *CSharpParserBase) LookAheadIs(pos int, value int) bool {
	return p.GetTokenStream().LA(pos) == value
}

func (p *CSharpParserBase) LookAheadIsNot(pos int, value int) bool {
	return p.GetTokenStream().LA(pos) != value
}

// -------------------------------------------------------------------------
// Error reporting
// -------------------------------------------------------------------------

func (p *CSharpParserBase) notifySemanticError(line int, charPositionInLine int, msg string) {
	p.NotifyErrorListeners(msg, nil, nil)
	fmt.Fprintf(os.Stderr, "%d:%d %s\n", line, charPositionInLine, msg)
}

// -------------------------------------------------------------------------
// ReduceTree — post-parse cleanup (for external tooling)
// -------------------------------------------------------------------------

func (p *CSharpParserBase) ReduceTree(currentctx antlr.ParserRuleContext) {
	ruleNames := p.GetRuleNames()
	ruleTypeArgumentList := -1
	for i, name := range ruleNames {
		if name == "type_argument_list" {
			ruleTypeArgumentList = i
			break
		}
	}

	var takeOutEmpties func(node antlr.ParserRuleContext)
	takeOutEmpties = func(node antlr.ParserRuleContext) {
		if node == nil {
			return
		}
		base, ok := node.(*antlr.BaseParserRuleContext)
		if !ok || base == nil {
			return
		}
		for i := node.GetChildCount() - 1; i >= 0; i-- {
			child := node.GetChild(i)
			childRule, ok := child.(antlr.ParserRuleContext)
			if !ok {
				continue
			}
			if childRule.GetRuleIndex() == ruleTypeArgumentList && childRule.GetChildCount() == 0 {
				base.RemoveLastChild()
			} else {
				takeOutEmpties(childRule)
			}
		}
	}
	takeOutEmpties(currentctx)
}

// -------------------------------------------------------------------------
// insertNode — rewire parse tree (MLR un-inlining)
// -------------------------------------------------------------------------

func (p *CSharpParserBase) insertNode(currentctx antlr.ParserRuleContext, contextTypeName string) {
	// Collect existing children
	n := currentctx.GetChildCount()
	oldChildren := make([]antlr.Tree, n)
	for i := 0; i < n; i++ {
		oldChildren[i] = currentctx.GetChild(i)
	}

	// Create the new context node using a registry of known types
	var inserted antlr.ParserRuleContext
	switch contextTypeName {
	case "Invocation_expressionContext":
		inserted = NewInvocation_expressionContext(p, currentctx, currentctx.GetInvokingState())
	case "Element_accessContext":
		inserted = NewElement_accessContext(p, currentctx, currentctx.GetInvokingState())
	case "Member_accessContext":
		inserted = NewMember_accessContext(p, currentctx, currentctx.GetInvokingState())
	case "Null_conditional_member_accessContext":
		inserted = NewNull_conditional_member_accessContext(p, currentctx, currentctx.GetInvokingState())
	case "Null_conditional_element_accessContext":
		inserted = NewNull_conditional_element_accessContext(p, currentctx, currentctx.GetInvokingState())
	case "Post_increment_expressionContext":
		inserted = NewPost_increment_expressionContext(p, currentctx, currentctx.GetInvokingState())
	case "Post_decrement_expressionContext":
		inserted = NewPost_decrement_expressionContext(p, currentctx, currentctx.GetInvokingState())
	case "Null_forgiving_expressionContext":
		inserted = NewNull_forgiving_expressionContext(p, currentctx, currentctx.GetInvokingState())
	case "Pointer_member_accessContext":
		inserted = NewPointer_member_accessContext(p, currentctx, currentctx.GetInvokingState())
	default:
		return
	}

	// Remove all children from currentctx
	for i := 0; i < n; i++ {
		currentctx.RemoveLastChild()
	}

	// Add old children to inserted
	for _, child := range oldChildren {
		if rc, ok := child.(antlr.RuleContext); ok {
			inserted.AddChild(rc)
		} else if tn, ok := child.(antlr.TerminalNode); ok {
			inserted.AddTokenNode(tn.GetSymbol())
		}
	}

	// Add inserted as the only child of currentctx
	currentctx.AddChild(inserted)
}

func (p *CSharpParserBase) AsInvocationExpression(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Invocation_expressionContext")
}

func (p *CSharpParserBase) AsElementAccess(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Element_accessContext")
}

func (p *CSharpParserBase) AsMemberAccess(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Member_accessContext")
}

func (p *CSharpParserBase) AsNullConditionalMemberAccess(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Null_conditional_member_accessContext")
}

func (p *CSharpParserBase) AsNullConditionalElementAccess(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Null_conditional_element_accessContext")
}

func (p *CSharpParserBase) AsPostIncrementExpression(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Post_increment_expressionContext")
}

func (p *CSharpParserBase) AsPostDecrementExpression(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Post_decrement_expressionContext")
}

func (p *CSharpParserBase) AsNullForgivingExpression(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Null_forgiving_expressionContext")
}

func (p *CSharpParserBase) AsPointerMemberAccess(currentctx antlr.ParserRuleContext) {
	p.insertNode(currentctx, "Pointer_member_accessContext")
}

// -------------------------------------------------------------------------
// ElementAccessSemanticCheck
// -------------------------------------------------------------------------

func (p *CSharpParserBase) ElementAccessSemanticCheck(currentctx antlr.ParserRuleContext) {
	ruleNames := p.GetRuleNames()

	ruleIdx := func(name string) int {
		for i, n := range ruleNames {
			if n == name {
				return i
			}
		}
		return -1
	}

	rulePrimaryExpression            := ruleIdx("primary_expression")
	ruleElementAccess                := ruleIdx("element_access")
	rulePointerElementAccess         := ruleIdx("pointer_element_access")
	ruleNullConditionalElementAccess := ruleIdx("null_conditional_element_access")
	ruleArrayCreationExpression      := ruleIdx("array_creation_expression")
	ruleStackallocExpression         := ruleIdx("stackalloc_expression")

	if currentctx == nil || currentctx.GetRuleIndex() != rulePrimaryExpression || currentctx.GetChildCount() != 1 {
		return
	}

	childTree := currentctx.GetChild(0)
	childRule, ok := childTree.(antlr.ParserRuleContext)
	if !ok {
		return
	}

	childRuleIndex  := childRule.GetRuleIndex()
	childChildCount := childRule.GetChildCount()

	if childRuleIndex == ruleElementAccess || childRuleIndex == rulePointerElementAccess {
		if childChildCount != 4 {
			return
		}
	} else if childRuleIndex == ruleNullConditionalElementAccess {
		if childChildCount != 5 {
			return
		}
	} else {
		return
	}

	accessTargetTree := childRule.GetChild(0)
	accessTarget, ok := accessTargetTree.(antlr.ParserRuleContext)
	if !ok {
		return
	}
	if accessTarget.GetRuleIndex() != rulePrimaryExpression || accessTarget.GetChildCount() == 0 {
		return
	}

	lhsTargetTree := accessTarget.GetChild(0)
	lhsTarget, ok := lhsTargetTree.(antlr.ParserRuleContext)
	if !ok {
		return
	}

	if lhsTarget.GetRuleIndex() != ruleArrayCreationExpression &&
		lhsTarget.GetRuleIndex() != ruleStackallocExpression {
		return
	}

	lhsLast := lhsTarget.GetStop()
	if lhsLast == nil {
		return
	}
	lhsLastType := lhsLast.GetTokenType()

	if lhsLastType == CSharpLexerTK_RBRACE {
		return // initializer present
	}

	if lhsLastType != CSharpLexerTK_RBRACK {
		fmt.Fprintf(os.Stderr, "%d:%d Error: Unexpected LHS last token %s (%d).\n",
			lhsLast.GetLine(), lhsLast.GetColumn(), lhsLast.GetText(), lhsLastType)
		return
	}

	childRuleName := ruleNames[childRuleIndex]
	lhsRuleName   := ruleNames[lhsTarget.GetRuleIndex()]
	childPrefix := "a"
	if len(childRuleName) > 0 && strings.ContainsRune("AEIOUaeiou", rune(childRuleName[0])) {
		childPrefix = "an"
	}
	lhsPrefix := "a"
	if len(lhsRuleName) > 0 && strings.ContainsRune("AEIOUaeiou", rune(lhsRuleName[0])) {
		lhsPrefix = "an"
	}

	p.notifySemanticError(lhsLast.GetLine(), lhsLast.GetColumn(),
		fmt.Sprintf("LHS of %s %s cannot be %s %s unless it has an initializer",
			childPrefix, childRuleName, lhsPrefix, lhsRuleName))
}
