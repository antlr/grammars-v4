#include "antlr4-runtime.h"
#include "RustParserBase.h"
#include "RustParser.h"

bool RustParserBase::NextGT()
{
    return _input->LA(1) == RustParser::GT;
}

bool RustParserBase::NextLT()
{
    return _input->LA(1) == RustParser::LT;
}
