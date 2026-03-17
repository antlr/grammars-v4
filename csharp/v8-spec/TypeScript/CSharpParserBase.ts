import { Parser, ParserRuleContext, TokenStream } from "antlr4";
import CSharpLexer from "./CSharpLexer.js";

export default abstract class CSharpParserBase extends Parser {

    constructor(input: TokenStream) {
        super(input);
    }

    // -------------------------------------------------------------------------
    // Look-ahead helpers
    // -------------------------------------------------------------------------

    LookAheadIs(pos: number, value: number): boolean {
        return this._input.LA(pos) === value;
    }

    LookAheadIsNot(pos: number, value: number): boolean {
        return this._input.LA(pos) !== value;
    }

    // -------------------------------------------------------------------------
    // Error reporting
    // -------------------------------------------------------------------------

    private _notifySemanticError(line: number, charPositionInLine: number, msg: string): void {
        this.notifyErrorListeners(msg, this.getCurrentToken(), undefined);
    }

    // -------------------------------------------------------------------------
    // ReduceTree — post-parse cleanup (for external tooling)
    // -------------------------------------------------------------------------

    ReduceTree(currentctx: ParserRuleContext): void {
        const ruleTypeArgumentList = (this as any).ruleNames.indexOf("type_argument_list");

        const takeOutEmpties = (node: ParserRuleContext): void => {
            if (!node.children) return;
            for (let ix = node.children.length - 1; ix >= 0; ix--) {
                const child = node.children[ix];
                if (child instanceof ParserRuleContext) {
                    if ((child as any).ruleIndex === ruleTypeArgumentList && child.getChildCount() === 0)
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
        const CtxCls = (this.constructor as any)[contextTypeName] as (new (parent: ParserRuleContext, invokingState: number) => ParserRuleContext) | undefined;
        if (!CtxCls) return;
        const inserted = new CtxCls(currentctx, (currentctx as any).invokingState ?? -1);
        (inserted as any).children = (currentctx as any).children ? [...(currentctx as any).children] : [];
        (currentctx as any).children = [inserted];
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
        const rIdx = (name: string): number => (this as any).ruleNames.indexOf(name);

        const rulePrimaryExpression            = rIdx("primary_expression");
        const ruleElementAccess                = rIdx("element_access");
        const rulePointerElementAccess         = rIdx("pointer_element_access");
        const ruleNullConditionalElementAccess = rIdx("null_conditional_element_access");
        const ruleArrayCreationExpression      = rIdx("array_creation_expression");
        const ruleStackallocExpression         = rIdx("stackalloc_expression");

        if ((currentctx as any).ruleIndex !== rulePrimaryExpression || currentctx.getChildCount() !== 1) return;

        const childRule = currentctx.getChild(0);
        if (!(childRule instanceof ParserRuleContext)) return;

        const childRuleIndex  = (childRule as any).ruleIndex as number;
        const childChildCount = childRule.getChildCount();

        if (childRuleIndex === ruleElementAccess || childRuleIndex === rulePointerElementAccess) {
            if (childChildCount !== 4) return;
        } else if (childRuleIndex === ruleNullConditionalElementAccess) {
            if (childChildCount !== 5) return;
        } else return;

        const accessTargetTree = childRule.getChild(0);
        if (!(accessTargetTree instanceof ParserRuleContext)) return;
        if ((accessTargetTree as any).ruleIndex !== rulePrimaryExpression || accessTargetTree.getChildCount() === 0) return;

        const lhsTargetTree = accessTargetTree.getChild(0);
        if (!(lhsTargetTree instanceof ParserRuleContext)) return;

        const lhsRuleIndex = (lhsTargetTree as any).ruleIndex as number;
        if (lhsRuleIndex !== ruleArrayCreationExpression && lhsRuleIndex !== ruleStackallocExpression) return;

        const lhsLast     = (lhsTargetTree as any).stop;
        const lhsLastType = lhsLast ? lhsLast.type : -1;

        if (lhsLastType === CSharpLexer.TK_RBRACE) return; // initializer present

        if (lhsLastType !== CSharpLexer.TK_RBRACK) {
            process.stderr.write(
                `${lhsLast?.line}:${lhsLast?.column} Error: Unexpected LHS last token ${lhsLast?.text} (${lhsLastType}).\n`);
            return;
        }

        const childRuleName = (this as any).ruleNames[childRuleIndex];
        const lhsRuleName   = (this as any).ruleNames[lhsRuleIndex];
        const childPrefix   = "AEIOUaeiou".includes(childRuleName[0]) ? "an" : "a";
        const lhsPrefix     = "AEIOUaeiou".includes(lhsRuleName[0])   ? "an" : "a";

        this._notifySemanticError(lhsLast?.line ?? 0, lhsLast?.column ?? 0,
            `LHS of ${childPrefix} ${childRuleName} cannot be ${lhsPrefix} ${lhsRuleName} unless it has an initializer`);
    }
}
