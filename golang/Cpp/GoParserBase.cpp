#include "GoParserBase.h"
#include "GoParser.h"

bool GoParserBase::closingBracket()
{
    antlr4::BufferedTokenStream* stream = static_cast<antlr4::BufferedTokenStream*>(_input);
    int prevTokenType = stream->LA(1);

    return prevTokenType == GoParser::R_CURLY || prevTokenType == GoParser::R_PAREN;
}
