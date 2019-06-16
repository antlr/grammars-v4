#pragma once

#include "antlr4-runtime.h"

class JavaScriptBaseParser : public antlr4::Parser {
public:
    JavaScriptBaseParser(antlr4::TokenStream *input);
    bool p(std::string str);
    bool prev(std::string str);
    bool n(std::string str);
    bool next(std::string str);
    bool notLineTerminator();
    bool notOpenBraceAndNotFunction();
    bool closeBrace();
    bool here(int type);
    bool lineTerminatorAhead();
};
