#pragma once

#include <stack>

#include "antlr4-runtime.h"

class JavaScriptLexerBase : public antlr4::Lexer {
public:
    JavaScriptLexerBase(antlr4::CharStream *input);

    std::stack<bool> scopeStrictModes;
    
    bool lastToken = false;
    size_t lastTokenType = 0;

    bool useStrictDefault = false;
    bool useStrictCurrent = false;

    bool IsStartOfFile();
    bool getStrictDefault();
    void setUseStrictDefault(bool value);
    bool IsStrictMode();
    virtual std::unique_ptr<antlr4::Token> nextToken() override;
    void ProcessOpenBrace();
    void ProcessCloseBrace();
    void ProcessStringLiteral();
    bool IsRegexPossible();
};
