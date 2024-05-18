#pragma once

#include "antlr4-runtime.h"

class CPP14ParserBase : public antlr4::Parser {
public:
    CPP14ParserBase(antlr4::TokenStream *input) : Parser(input) { }
    bool IsPureSpecifierAllowed();
};
