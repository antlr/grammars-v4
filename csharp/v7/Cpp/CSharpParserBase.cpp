#include "antlr4-runtime.h"
#include "CSharpParserBase.h"
#include "CSharpParser.h"
#include <algorithm>
#include <cctype>

CSharpParserBase::CSharpParserBase(antlr4::TokenStream *input)
    : antlr4::Parser(input)
{
}

const std::vector<std::string> &CSharpParserBase::allSemanticFunctions()
{
    static const std::vector<std::string> funcs = {"IsLocalVariableDeclaration"};
    return funcs;
}

void CSharpParserBase::initSemantics()
{
    if (initialized_)
        return;
    initialized_ = true;
    // NOTE: argc/argv are not available from the parser base class.
    // The test harness (Test.cpp) should call noSemantics_.insert() after
    // construction if --no-semantics was passed on the command line.
}

bool CSharpParserBase::IsLocalVariableDeclaration()
{
    initSemantics();
    if (noSemantics_.count("IsLocalVariableDeclaration"))
        return true;

    auto *ctx = dynamic_cast<CSharpParser::Local_variable_declarationContext *>(getRuleContext());
    if (ctx == nullptr)
        return true;
    auto *local_variable_type = ctx->local_variable_type();
    if (local_variable_type == nullptr)
        return true;
    if (local_variable_type->getText() == "var")
        return false;
    return true;
}
