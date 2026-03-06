#pragma once
#include "antlr4-runtime.h"
#include <deque>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

class CSharpLexerBase : public antlr4::Lexer
{
public:
    CSharpLexerBase(antlr4::CharStream *input);

    // Interpolated string helpers
    void OnInterpolatedRegularStringStart();
    void OnInterpolatedVerbatiumStringStart();
    void OnOpenBrace();
    void OnCloseBrace();
    void OnColon();
    void OpenBraceInside();
    void OnDoubleQuoteInside();
    void OnCloseBraceInside();
    bool IsRegularCharInside();
    bool IsVerbatiumDoubleQuoteInside();

    std::unique_ptr<antlr4::Token> nextToken() override;

    // Call from Test.cpp for each --DSYM argument
    void addSymbol(const std::string &sym) { symbols_.insert(sym); }

protected:
    int interpolatedStringLevel = 0;
    std::vector<bool> interpolatedVerbatiums;
    std::vector<int> curlyLevels;
    bool verbatium = false;

private:
    // Preprocessor state
    std::deque<std::unique_ptr<antlr4::Token>> pending_;
    std::unordered_set<std::string> symbols_;
    std::vector<bool> condition_;
    std::vector<bool> taken_;

    // Expression evaluator — tokens owned by lineStorage_
    std::vector<std::unique_ptr<antlr4::Token>> lineStorage_;
    std::vector<antlr4::Token *>                exprTokens_;
    int epos_ = 0;

    bool isActive() const;

    void handleDefine();
    void handleUndef();
    std::unique_ptr<antlr4::Token> handleIf();
    std::unique_ptr<antlr4::Token> handleElif();
    std::unique_ptr<antlr4::Token> handleElse();
    void handleEndif();

    // collectLine owns the returned tokens via lineStorage_
    std::vector<antlr4::Token *> collectLine();
    std::string symbolFromLine(const std::vector<antlr4::Token *> &line);
    std::unique_ptr<antlr4::Token> skipFalseBlock();
    std::string peekKeyword();

    bool evaluate(const std::vector<antlr4::Token *> &tokens);
    int  peekType();
    antlr4::Token *eConsume();
    bool parseOr();
    bool parseAnd();
    bool parseEq();
    bool parseUnary();
    bool parsePrimary();
};
