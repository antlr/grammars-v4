#pragma once

#include "antlr4-runtime.h"

class Python3ParserBase : public antlr4::Parser {
public:
    Python3ParserBase(antlr4::TokenStream *input) : Parser(input) { }
    bool CannotBePlusMinus();
    bool CannotBeDotLpEq();

};
