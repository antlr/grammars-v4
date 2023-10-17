#include "JavaScriptParser.h"

using namespace antlr4;

bool JavaScriptParserBase::p(std::string str)
{
    return prev(str);
}

bool JavaScriptParserBase::prev(std::string str)
{
    return _input->LT(-1)->getText() == str;
}

bool JavaScriptParserBase::n(std::string str)
{
    return next(str);
}

bool JavaScriptParserBase::next(std::string str)
{
    return _input->LT(1)->getText() == str;
}

bool JavaScriptParserBase::notLineTerminator()
{
    return !here(JavaScriptParser::LineTerminator);
}

bool JavaScriptParserBase::notOpenBraceAndNotFunction()
{
    int nextTokenType = _input->LT(1)->getType();
    return nextTokenType != JavaScriptParser::OpenBrace && nextTokenType != JavaScriptParser::Function_;

}

bool JavaScriptParserBase::closeBrace()
{
    return _input->LT(1)->getType() == JavaScriptParser::CloseBrace;
}

bool JavaScriptParserBase::here(int type)
{
    // Get the most recently emitted token.
    auto currentToken = _input->LT(-1);

    // Get the next token index.
    int nextTokenIndex = currentToken == nullptr ? 0 : currentToken->getTokenIndex() + 1;

    // Get the token after the `currentToken`. By using `_input.get(index)`,
    // we also grab a token that is (possibly) on the HIDDEN channel.
    auto nextToken = _input->get(nextTokenIndex);

    // Check if the token resides on the HIDDEN channel and if it's of the
    // provided type.
    return (nextToken->getChannel() == Lexer::HIDDEN) && (nextToken->getType() == type);
}

bool JavaScriptParserBase::lineTerminatorAhead()
{
    // Get the token ahead of the current index.
    int possibleIndexEosToken = this->getCurrentToken()->getTokenIndex() - 1;
    if (possibleIndexEosToken < 0) return false;
    auto ahead = _input->get(possibleIndexEosToken);

    if (ahead->getChannel() != Lexer::HIDDEN) {
        // We're only interested in tokens on the HIDDEN channel.
        return false;
    }

    if (ahead->getType() == JavaScriptParser::LineTerminator) {
        // There is definitely a line terminator ahead.
        return true;
    }

    if (ahead->getType() == JavaScriptParser::WhiteSpaces) {
        // Get the token ahead of the current whitespaces.
        possibleIndexEosToken = this->getCurrentToken()->getTokenIndex() - 2;
        if (possibleIndexEosToken < 0) return false;
        ahead = _input->get(possibleIndexEosToken);
    }

    // Get the token's text and type.
    std::string text = ahead->getText();
    int type = ahead->getType();

    // Check if the token is, or contains a line terminator.
    return (type == JavaScriptParser::MultiLineComment && (text.find("\r") != std::string::npos || text.find("\n") != std::string::npos)) ||
            (type == JavaScriptParser::LineTerminator);
}
