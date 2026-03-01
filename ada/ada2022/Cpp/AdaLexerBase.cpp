#include "antlr4-runtime.h"
#include "AdaLexerBase.h"
#include "AdaLexer.h"

AdaLexerBase::AdaLexerBase(antlr4::CharStream * input) : antlr4::Lexer(input)
{
}

std::unique_ptr<antlr4::Token> AdaLexerBase::nextToken()
{
    auto token = antlr4::Lexer::nextToken();
    if (token->getChannel() == antlr4::Token::DEFAULT_CHANNEL) {
        _lastTokenType = token->getType();
    }
    return token;
}

bool AdaLexerBase::IsCharLiteralAllowed()
{
    // In Ada, a tick after an identifier, closing paren, or 'all' keyword
    // is an attribute tick, not the start of a character literal.
    return _lastTokenType != AdaLexer::IDENTIFIER_
        && _lastTokenType != AdaLexer::RP
        && _lastTokenType != AdaLexer::ALL;
}
