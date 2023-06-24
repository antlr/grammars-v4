#pragma once

#include <stack>

#include "antlr4-runtime.h"

class Python3LexerBase : public antlr4::Lexer {
public:
    Python3LexerBase(antlr4::CharStream *input): Lexer(input) { }

    std::vector<std::unique_ptr<antlr4::Token>> tokens;
    std::deque<int> indents;
    int opened;
    antlr4::Token* lastToken;

    virtual void emit(std::unique_ptr<antlr4::Token> newToken) override;
    virtual std::unique_ptr<antlr4::Token> nextToken() override;
    antlr4::Token createDedent();
    antlr4::CommonToken commonToken(int type, std::string text);
    static int getIndentationCount(std::string spaces);
    bool atStartOfInput();
    void openBrace();
    void closeBrace();
    void onNewLine();
    virtual void reset() override;

    std::unique_ptr<antlr4::CommonToken> cloneToken(const std::unique_ptr<antlr4::Token>& source);

};
