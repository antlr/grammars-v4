#include "TypeScriptLexerBase.h"
#include "TypeScriptLexer.h"
using namespace antlr4;

TypeScriptLexerBase::TypeScriptLexerBase(CharStream *input) : Lexer(input)
{
}

bool TypeScriptLexerBase::getStrictDefault()
{
    return useStrictDefault;
}

void TypeScriptLexerBase::setUseStrictDefault(bool value)
{
    useStrictDefault = value;
    useStrictCurrent = value;
}

bool TypeScriptLexerBase::IsStrictMode()
{
    return useStrictCurrent;
}

std::unique_ptr<antlr4::Token> TypeScriptLexerBase::nextToken()
{
    auto next = Lexer::nextToken();

    if (next->getChannel() == Token::DEFAULT_CHANNEL)
    {
        // Keep track of the last token on the default channel.
        lastToken = true;
        lastTokenType = next->getType();
    }

    return next;
}

void TypeScriptLexerBase::ProcessOpenBrace()
{
    bracesDepth++;
    useStrictCurrent = scopeStrictModes.size() > 0 && scopeStrictModes.top() ? true : useStrictDefault;
    scopeStrictModes.push(useStrictCurrent);
}

void TypeScriptLexerBase::ProcessCloseBrace()
{
    bracesDepth--;

    if (scopeStrictModes.size() > 0)
    {
        useStrictCurrent = scopeStrictModes.top();
        scopeStrictModes.pop();
    }
    else
    {
        useStrictCurrent = useStrictDefault;
    }
}

void TypeScriptLexerBase::ProcessStringLiteral()
{
    if (lastToken || lastTokenType == TypeScriptLexer::OpenBrace)
    {
        std::string text = getText();
        if (text == "\"use strict\"" || text == "'use strict'")
        {
            if (scopeStrictModes.size() > 0)
                scopeStrictModes.pop();
            useStrictCurrent = true;
            scopeStrictModes.push(useStrictCurrent);
        }
    }
}

bool TypeScriptLexerBase::IsRegexPossible()
{
    if (lastToken)
    {
        // No token has been produced yet: at the start of the input,
        // no division is possible, so a regex literal _is_ possible.
        return true;
    }

    switch (lastTokenType)
    {
    case TypeScriptLexer::Identifier:
    case TypeScriptLexer::NullLiteral:
    case TypeScriptLexer::BooleanLiteral:
    case TypeScriptLexer::This:
    case TypeScriptLexer::CloseBracket:
    case TypeScriptLexer::CloseParen:
    case TypeScriptLexer::OctalIntegerLiteral:
    case TypeScriptLexer::DecimalLiteral:
    case TypeScriptLexer::HexIntegerLiteral:
    case TypeScriptLexer::StringLiteral:
    case TypeScriptLexer::PlusPlus:
    case TypeScriptLexer::MinusMinus:
        // After any of the tokens above, no regex literal can follow.
        return false;
    default:
        // In all other cases, a regex literal _is_ possible.
        return true;
    }
}

void TypeScriptLexerBase::StartTemplateString() { bracesDepth = 0; }

bool TypeScriptLexerBase::IsInTemplateString() { return templateDepth > 0 && bracesDepth == 0; }

void TypeScriptLexerBase::IncreaseTemplateDepth() { ++templateDepth; }

void TypeScriptLexerBase::DecreaseTemplateDepth() { --templateDepth; }
