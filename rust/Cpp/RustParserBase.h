#pragma once

#include "antlr4-runtime.h"

class RustParserBase : public antlr4::Parser {
public:
    RustParserBase(antlr4::TokenStream* input) : Parser(input) {
    }

    virtual ~RustParserBase() {}

    bool next(char expect) {
        return _input->LA(1) == expect;
    }
};
