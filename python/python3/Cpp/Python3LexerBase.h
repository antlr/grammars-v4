#pragma once

#include <stack>

#include "antlr4-runtime.h"

class Python3LexerBase : public antlr4::Lexer {
public:
    Python3LexerBase(antlr4::CharStream *input);

    std::vector<std::unique_ptr<antlr4::Token>> tokens;
    std::stack<int> indents;
    int opened;
    std::unique_ptr<antlr4::Token> lastToken;

    virtual void emit(std::unique_ptr<antlr4::Token> newToken) override;
    virtual std::unique_ptr<antlr4::Token> nextToken() override;
    std::unique_ptr<antlr4::Token> createDedent();
    std::unique_ptr<antlr4::CommonToken> commonToken(size_t type, const std::string& text);
    static int getIndentationCount(const std::string& spaces);
    bool atStartOfInput();
    void openBrace();
    void closeBrace();
    void onNewLine();
    virtual void reset() override;
    std::unique_ptr<antlr4::CommonToken> cloneToken(const std::unique_ptr<antlr4::Token>& source);
};
