#pragma once
#include "antlr4-runtime.h"

class AdaLexerBase : public antlr4::Lexer
{
private:
    int _lastTokenType = 0;

public:
    AdaLexerBase(antlr4::CharStream * input);
    std::unique_ptr<antlr4::Token> nextToken() override;
    bool IsCharLiteralAllowed();
};
