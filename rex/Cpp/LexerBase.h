#pragma once

#include "antlr4-runtime.h"

class LexerBase : public antlr4::Lexer {
public:
    explicit LexerBase(antlr4::CharStream *input) : antlr4::Lexer(input) {}
    bool Check1() { return _input->LA(1) != ':'; }
};
