#pragma once
#include "antlr4-runtime.h"
#include <string>
#include <unordered_set>
#include <vector>

class CSharpParser;

class CSharpParserBase : public antlr4::Parser
{
public:
    CSharpParserBase(antlr4::TokenStream *input);

    bool IsRightArrow();
    bool IsRightShift();
    bool IsRightShiftAssignment();
    bool IsLocalVariableDeclaration();

protected:
    std::unordered_set<std::string> noSemantics_;

private:
    bool initialized_ = false;
    void initSemantics();
    bool areAdjacent();

    static const std::vector<std::string> &allSemanticFunctions();
};
