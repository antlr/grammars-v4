#pragma once

#include "antlr4-runtime.h"

class RustLexerBase : public antlr4::Lexer {
    size_t lt1;
    size_t lt2;
public:
    RustLexerBase(antlr4::CharStream* input) : Lexer(input), lt1(antlr4::Token::INVALID_TYPE), lt2(antlr4::Token::INVALID_TYPE) { }
    virtual ~RustLexerBase() {}
    std::unique_ptr<antlr4::Token> nextToken() override;
    bool SOF();
    bool FloatDotPossible();
    bool FloatLiteralPossible();
};
