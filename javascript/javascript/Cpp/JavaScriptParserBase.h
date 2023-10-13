#pragma once

#include "antlr4-runtime.h"

class JavaScriptParserBase : public antlr4::Parser {
public:
    JavaScriptParserBase(antlr4::TokenStream *input) : Parser(input) { }
    bool p(std::string str);
    bool prev(std::string str);
    bool n(std::string str);
    bool next(std::string str);
    bool notLineTerminator();
    bool notOpenBraceAndNotFunction();
    bool closeBrace();
    bool here(int type);
    bool lineTerminatorAhead();
    bool notLineTerminatorAhead();
};
