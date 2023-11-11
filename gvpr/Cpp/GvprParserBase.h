#pragma once

#include "antlr4-runtime.h"

class GvprParserBase : public antlr4::Parser {
public:
    GvprParserBase(antlr4::TokenStream *input) : Parser(input) { }
    bool IsSemiRequired();
    bool IsSemiNotRequired();
};
