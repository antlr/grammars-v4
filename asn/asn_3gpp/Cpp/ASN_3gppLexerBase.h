#ifndef ASN_3GPPLEXERBASE_H
#define ASN_3GPPLEXERBASE_H

#include "antlr4-runtime.h"

class ASN_3gppLexerBase : public antlr4::Lexer
{
public:
    ASN_3gppLexerBase(antlr4::CharStream *input) : Lexer(input), self(*this) { }

public:
    ASN_3gppLexerBase & self;

public:
    bool IsColumnZero()
    {
        return this->getCharPositionInLine() == 0;
    }
};

#endif