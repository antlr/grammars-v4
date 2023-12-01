#include "PythonParser.h"

using namespace antlr4;

bool PythonParserBase::CannotBePlusMinus()
{
    return true;
}

bool PythonParserBase::CannotBeDotLpEq()
{
    return true;
}
