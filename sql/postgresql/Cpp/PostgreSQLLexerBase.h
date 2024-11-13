#pragma once
#include "antlr4-runtime.h"
#include <string>
#include <stack>

class PostgreSQLLexerBase : public antlr4::Lexer
{
    public:
        PostgreSQLLexerBase(antlr4::CharStream * input);
	void PushTag();
	bool IsTag();
	void PopTag();
	void UnterminatedBlockCommentDebugAssert();
	bool CheckLaMinus();
	bool CheckLaStar();
	bool CharIsLetter();
	void HandleNumericFail();
	void HandleLessLessGreaterGreater();
	bool CheckIfUtf32Letter();
	bool IsSemiColon();
    private:
        std::stack<std::string> tags;

};
