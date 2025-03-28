#include "GoParserBase.h"
#include "GoParser.h"

bool GoParserBase::closingBracket()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    int nextTokenType = stream->LA(1);
    return nextTokenType == GoParser::R_CURLY || nextTokenType == GoParser::R_PAREN;
}

bool GoParserBase::isType()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    int nextTokenType = stream->LA(1);
    return nextTokenType != GoParser::IDENTIFIER;
}

