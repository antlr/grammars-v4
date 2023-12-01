#pragma once

#include "antlr4-runtime.h"

class PythonParserBase : public antlr4::Parser {
public:
	PythonParserBase(antlr4::TokenStream *input) : Parser(input) { }
    bool CannotBePlusMinus();
    bool CannotBeDotLpEq();

};
