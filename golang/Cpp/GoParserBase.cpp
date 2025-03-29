#include "GoParserBase.h"
#include "GoParser.h"

bool GoParserBase::closingBracket()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    int la = stream->LA(1);
    return la == GoParser::R_CURLY || la == GoParser::R_PAREN || la == antlr4::Token::EOF;
}

bool GoParserBase::isType()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    int la = stream->LA(1);
    return la != GoParser::IDENTIFIER;
}

bool GoParserBase::isNotReceive()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    int la = stream->LA(2);
    return la != GoParser::RECEIVE;
}

