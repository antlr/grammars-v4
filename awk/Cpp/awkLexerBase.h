#pragma once

#include "antlr4-runtime.h"

class awkLexerBase : public antlr4::Lexer {
    bool _afterExpr;
public:
    awkLexerBase(antlr4::CharStream* input) : Lexer(input) { }
    virtual ~awkLexerBase() {}
    std::unique_ptr<antlr4::Token> nextToken() override;
    bool IsNotAfterExpr();
};
