#pragma once
#include "antlr4-runtime.h"

class PostgreSQLLexerBase : public antlr4::Lexer
{
    public:
        PostgreSQLLexerBase(antlr4::CharStream * input);
	bool CheckLaMinus();
	bool CheckLaStar();
	void HandleLessLessGreaterGreater();
	bool CharIsLetter();
	bool CheckIfUtf32Letter();
	void PushTag();
	bool IsTag();
	void PopTag();
	void HandleNumericFail();
	void UnterminatedBlockCommentDebugAssert();
	bool IsSemiColon();
};
