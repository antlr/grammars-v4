#ifndef PLSQLLEXERBASE_H
#define PLSQLLEXERBASE_H

#include "antlr4-runtime.h"

class Fortran77LexerBase : public antlr4::Lexer
{
public:
    Fortran77LexerBase(antlr4::CharStream *input) : Lexer(input), self(*this) { }

public:
    Fortran77LexerBase & self;

public:
    bool IsColumnZero()
    {
        return this->getCharPositionInLine() == 0;
    }
};

#endif