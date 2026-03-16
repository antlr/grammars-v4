#pragma once
#include "antlr4-runtime.h"
#include <string>

class CSharpParserBase : public antlr4::Parser
{
public:
    CSharpParserBase(antlr4::TokenStream *input);

    bool LookAheadIs(int pos, int value);
    bool LookAheadIsNot(int pos, int value);

    void ReduceTree(antlr4::ParserRuleContext *currentctx);

    void AsInvocationExpression(antlr4::ParserRuleContext *currentctx);
    void AsElementAccess(antlr4::ParserRuleContext *currentctx);
    void AsMemberAccess(antlr4::ParserRuleContext *currentctx);
    void AsNullConditionalMemberAccess(antlr4::ParserRuleContext *currentctx);
    void AsNullConditionalElementAccess(antlr4::ParserRuleContext *currentctx);
    void AsPostIncrementExpression(antlr4::ParserRuleContext *currentctx);
    void AsPostDecrementExpression(antlr4::ParserRuleContext *currentctx);
    void AsNullForgivingExpression(antlr4::ParserRuleContext *currentctx);
    void AsPointerMemberAccess(antlr4::ParserRuleContext *currentctx);

    void ElementAccessSemanticCheck(antlr4::ParserRuleContext *currentctx);

private:
    void notifySemanticError(int line, int charPositionInLine, const std::string &msg);
    void insertNode(antlr4::ParserRuleContext *currentctx, const std::string &contextTypeName);
    int  ruleIdx(const std::string &name) const;
};
