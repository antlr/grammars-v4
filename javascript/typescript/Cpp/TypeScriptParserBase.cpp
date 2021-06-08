#include "TypeScriptParser.h"

using namespace antlr4;

TypeScriptParserBase::TypeScriptParserBase(TokenStream *input) : Parser(input)
{
}

bool TypeScriptParserBase::p(std::string str)
{
    return prev(str);
}

bool TypeScriptParserBase::prev(std::string str)
{
    return _input->LT(-1)->getText() == str;
}

bool TypeScriptParserBase::n(std::string str)
{
    return next(str);
}

bool TypeScriptParserBase::next(std::string str)
{
    return _input->LT(1)->getText() == str;
}

bool TypeScriptParserBase::notLineTerminator()
{
    return !here(TypeScriptParser::LineTerminator);
}

bool TypeScriptParserBase::notOpenBraceAndNotFunction()
{
    int nextTokenType = _input->LT(1)->getType();
    return nextTokenType != TypeScriptParser::OpenBrace && nextTokenType != TypeScriptParser::Function_;

}

bool TypeScriptParserBase::closeBrace()
{
    return _input->LT(1)->getType() == TypeScriptParser::CloseBrace;
}

bool TypeScriptParserBase::here(int type)
{
    // Get the token ahead of the current index.
    int possibleIndexEosToken = this->getCurrentToken()->getTokenIndex() - 1;
    auto ahead = _input->get(possibleIndexEosToken);

    // Check if the token resides on the HIDDEN channel and if it's of the
    // provided type.
    return (ahead->getChannel() == Lexer::HIDDEN) && (ahead->getType() == type);
}

bool TypeScriptParserBase::lineTerminatorAhead()
{
    // Get the token ahead of the current index.
    int possibleIndexEosToken = this->getCurrentToken()->getTokenIndex() - 1;
    auto ahead = _input->get(possibleIndexEosToken);

    if (ahead->getChannel() != Lexer::HIDDEN) {
        // We're only interested in tokens on the HIDDEN channel.
        return false;
    }

    if (ahead->getType() == TypeScriptParser::LineTerminator) {
        // There is definitely a line terminator ahead.
        return true;
    }

    if (ahead->getType() == TypeScriptParser::WhiteSpaces) {
        // Get the token ahead of the current whitespaces.
        possibleIndexEosToken = this->getCurrentToken()->getTokenIndex() - 2;
        ahead = _input->get(possibleIndexEosToken);
    }

    // Get the token's text and type.
    std::string text = ahead->getText();
    int type = ahead->getType();

    // Check if the token is, or contains a line terminator.
    return (type == TypeScriptParser::MultiLineComment && (text.find("\r") != std::string::npos || text.find("\n") != std::string::npos)) ||
            (type == TypeScriptParser::LineTerminator);
}
