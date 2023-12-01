#pragma once

#include <stack>

#include "antlr4-runtime.h"

class PythonLexerBase : public antlr4::Lexer {
public:
    PythonLexerBase(antlr4::CharStream *input);

    std::unique_ptr<antlr4::Token> lastToken;
    std::vector<int> _indentLengths;
    std::vector<std::unique_ptr<antlr4::Token>> _pendingTokens;
    int _previousPendingTokenType;
    int _lastPendingTokenTypeForDefaultChannel;
    int _opened;
    std::unique_ptr<antlr4::CommonToken> _curToken; // current (under processing) token
    std::unique_ptr<antlr4::Token> _ffgToken;      // following (look ahead) token

    void AddPendingToken(std::unique_ptr<antlr4::CommonToken> token);

	void CheckNextToken();
    void SetCurrentAndFollowingTokens();
    void HandleStartOfInput();
    void HandleNEWLINEtoken();
    void HandleSTRINGtoken();
    void HandleFSTRING_MIDDLE_token();
    void HandleEOFtoken();
	void HandleFORMAT_SPECIFICATION_MODE();
    void HandleFStringLexerModes();
    void HideAndAddPendingToken(std::unique_ptr<antlr4::CommonToken>);
    void InsertLeadingIndentToken();
    void ReportLexerError(std::string errMsg);

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
