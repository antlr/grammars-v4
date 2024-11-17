#pragma once

#include "antlr4-runtime.h"

class PostgreSQLParserBase : public antlr4::Parser {
public:
    PostgreSQLParserBase(antlr4::TokenStream *input) : Parser(input) { }
    void ParseRoutineBody();
    bool OnlyAcceptableOps();
};
