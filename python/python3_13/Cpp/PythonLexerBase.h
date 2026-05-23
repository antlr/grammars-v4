/*
The MIT License (MIT)
Copyright (c) 2021 Robert Einhorn

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
 */

/*
 *
 * Project      : Python Indent/Dedent handler for ANTLR4 grammars
 *
 * Developed by : Robert Einhorn, robert.einhorn.hu@gmail.com
 *
 */

 #pragma once 

 #include <stack>
 #include <memory>
 #include <map>
 #include <vector>
 #include <regex>
 
 #include "antlr4-runtime.h"
 
 class PythonLexerBase : public antlr4::Lexer {
 public: 
     explicit PythonLexerBase(antlr4::CharStream *input): antlr4::Lexer(input) {
         this->init();
     }
     virtual std::unique_ptr<antlr4::Token> nextToken() override;
     virtual void reset() override;
 private:
     std::unique_ptr<antlr4::Token> cloneToken(const std::unique_ptr<antlr4::Token> &source);
     std::unique_ptr<antlr4::Token> cloneToken(const std::unique_ptr<antlr4::Token> &source, size_t channel);
     std::unique_ptr<antlr4::Token> cloneToken(const std::unique_ptr<antlr4::Token> &source, const std::string &text);
     std::unique_ptr<antlr4::Token> cloneToken(const std::unique_ptr<antlr4::Token> &source, size_t channel, const std::string &text, size_t type);
     void init();
     void checkNextToken();
     void setCurrentAndFollowingTokens();
     void insertENCODINGtoken();
     std::string getEncodingName(const std::string &commentText);
     void handleStartOfInput();
     void insertLeadingIndentToken();
     void handleNEWLINEtoken();
     void insertIndentOrDedentToken(size_t indentLength);
     void checkCurToken();
     void appendToBraceExpression(const std::string &text);
     void incrementBraceStack();
     void decrementBraceStack();
     void setLexerModeAfterRBRACEtoken();
     void setLexerModeByFSTRING_STARTtoken();
     void setLexerModeByCOLONorCOLONEQUALtoken();
     void popByBRACE();
     void handleFSTRING_MIDDLEtokenWithDoubleBrace();
     void handleFSTRING_MIDDLEtokenWithQuoteAndLBrace();
     std::string getLastTwoCharsOfTheCurTokenText();
     void trimLastCharAddPendingTokenSetCurToken(size_t type, const std::string &text, size_t channel);
     void handleCOLONEQUALtokenInFString();
     void createNewCurToken(size_t type, const std::string &text, size_t channel);
     void pushLexerMode(size_t mode);
     void popLexerMode();
     void handleFORMAT_SPECIFICATION_MODE();
     bool isDictionaryComprehensionOrSetComprehension(const std::string &code);
     void insertTrailingTokens();
     void handleEOFtoken();
     void hideAndAddPendingToken(const std::unique_ptr<antlr4::Token> &token);
     void createAndAddPendingToken(size_t type, size_t channel, const std::string &text, const std::unique_ptr<antlr4::Token> &sampleToken);
     void createAndAddPendingToken(size_t type, size_t channel, const std::unique_ptr<antlr4::Token> &sampleToken);
     void addPendingToken(const std::unique_ptr<antlr4::Token> &token);
     size_t getIndentationLength(const std::string &identText);
     void reportLexerError(const std::string &errMsg);
     void reportError(const std::string &errMsg);
 
     // A stack that keeps track of the indentation lengths
     std::stack<size_t> indentLengthStack;
     // A list where tokens are waiting to be loaded into the token stream
     std::vector<std::unique_ptr<antlr4::Token>> pendingTokens;
     // last pending token types
     size_t previousPendingTokenType;
     size_t lastPendingTokenTypeFromDefaultChannel;
 
     // The amount of opened parentheses, square brackets or curly braces
     size_t opened;
     //  The amount of opened parentheses and square brackets in the current lexer mode
     std::vector<size_t> paren_or_bracket_openedStack;
     // A stack that stores expression(s) between braces in fstring
     std::vector<std::string> braceExpressionStack;
     std::string prevBraceExpression;
 
     // Instead of this._mode      (_mode is not implemented in each ANTLR4 runtime)
     size_t curLexerMode;
     // Instead of this._modeStack (_modeStack is not implemented in each ANTLR4 runtime)
     std::vector<size_t> lexerModeStack;
     bool wasSpaceIndentation;
     bool wasTabIndentation;
     bool wasIndentationMixedWithSpacesAndTabs;
 
     std::unique_ptr<antlr4::Token> curToken; // current (under processing) token
     std::unique_ptr<antlr4::Token> ffgToken; // following (look ahead) token
 
     const ssize_t INVALID_LENGTH = -1;
     const std::string ERR_TXT = " ERROR: ";
 };
