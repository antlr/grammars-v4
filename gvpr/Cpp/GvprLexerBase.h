#pragma once
#include "antlr4-runtime.h"

class GvprLexerBase : public antlr4::Lexer
{
    public:
        GvprLexerBase(antlr4::CharStream * input);
        bool IsColumnZero();
};
