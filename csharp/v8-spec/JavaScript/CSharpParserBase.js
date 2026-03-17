import antlr4 from 'antlr4';
import CSharpLexer from './CSharpLexer.js';

export default class CSharpParserBase extends antlr4.Parser {
    constructor(input) {
        super(input);
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

        if (lhsLastType === CSharpLexer.TK_RBRACE) return; // initializer present

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
}
