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

    // Mode-stack helpers
    int  PeekMode() const;
    size_t popMode() override;
    bool PeekModeIs(int mode) const;
    bool LookAheadIs(int pos, int value);
    bool LookAheadIsNot(int pos, int value);
    bool LookAheadIsRBrace1() const;
    bool LookAheadIsNotLBrace2() const;
    bool PeekModeIsIrsCont() const;
    bool PeekModeIsIvsCont() const;
    void WrapToken();

    std::unique_ptr<antlr4::Token> nextToken() override;

    // Call from Test.cpp for each --DSYM argument
    void addSymbol(const std::string &sym) { symbols_.insert(sym); }

private:
    // Preprocessor state
    antlr4::CharStream *charStream_;
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
