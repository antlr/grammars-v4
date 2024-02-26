#include "Python3Parser.h"

using namespace antlr4;

bool Python3ParserBase::CannotBePlusMinus()
{
    return true;
}

bool Python3ParserBase::CannotBeDotLpEq()
{
    return true;
}
