#include "JavaScriptLexer.h"

using namespace antlr4;

JavaScriptBaseLexer::JavaScriptBaseLexer(CharStream *input) : Lexer(input)
{
}

bool JavaScriptBaseLexer::getStrictDefault()
{
    return useStrictDefault;
}

void JavaScriptBaseLexer::setUseStrictDefault(bool value)
{
    useStrictDefault = value;
    useStrictCurrent = value;
}

bool JavaScriptBaseLexer::IsStrictMode()
{
    return useStrictCurrent;
}

std::unique_ptr<antlr4::Token> JavaScriptBaseLexer::nextToken() {
    auto next = Lexer::nextToken();

    if (next->getChannel() == Token::DEFAULT_CHANNEL) {
        // Keep track of the last token on the default channel.
        lastToken = true;
        lastTokenType = next->getType();
    }

    return next;
}

void JavaScriptBaseLexer::ProcessOpenBrace()
{
    useStrictCurrent = scopeStrictModes.size() > 0 && scopeStrictModes.top() ? true : useStrictDefault;
    scopeStrictModes.push(useStrictCurrent);
}

void JavaScriptBaseLexer::ProcessCloseBrace()
{
    if (scopeStrictModes.size() > 0) {
        useStrictCurrent = scopeStrictModes.top();
        scopeStrictModes.pop();
    } else {
        useStrictCurrent = useStrictDefault;
    }
}

void JavaScriptBaseLexer::ProcessStringLiteral()
{
    if (lastToken || lastTokenType == JavaScriptLexer::OpenBrace)
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

bool JavaScriptBaseLexer::IsRegexPossible()
{
    if (lastToken) {
        // No token has been produced yet: at the start of the input,
        // no division is possible, so a regex literal _is_ possible.
        return true;
    }
    
    switch (lastTokenType) {
        case JavaScriptLexer::Identifier:
        case JavaScriptLexer::NullLiteral:
        case JavaScriptLexer::BooleanLiteral:
        case JavaScriptLexer::This:
        case JavaScriptLexer::CloseBracket:
        case JavaScriptLexer::CloseParen:
        case JavaScriptLexer::OctalIntegerLiteral:
        case JavaScriptLexer::DecimalLiteral:
        case JavaScriptLexer::HexIntegerLiteral:
        case JavaScriptLexer::StringLiteral:
        case JavaScriptLexer::PlusPlus:
        case JavaScriptLexer::MinusMinus:
            // After any of the tokens above, no regex literal can follow.
            return false;
        default:
            // In all other cases, a regex literal _is_ possible.
            return true;
    }
}
