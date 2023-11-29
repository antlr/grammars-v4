#pragma once
#include "antlr4-runtime.h"

class Dart2LexerBase : public antlr4::Lexer
{
    public:
        Dart2LexerBase(antlr4::CharStream * input);
        bool CheckNotOpenBrace();
};
