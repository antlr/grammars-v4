#include "antlr4-runtime.h"
#include "PostgreSQLLexerBase.h"
#include "PostgreSQLLexer.h"

PostgreSQLLexerBase::PostgreSQLLexerBase(antlr4::CharStream * input) : antlr4::Lexer(input)
{
    _input = input;
}

void PostgreSQLLexerBase::PushTag()
{
}

bool PostgreSQLLexerBase::IsTag()
{
	return true;
}

void PostgreSQLLexerBase::PopTag()
{
}

void PostgreSQLLexerBase::UnterminatedBlockCommentDebugAssert()
{
}

bool PostgreSQLLexerBase::CheckLaMinus()
{
	return this->getInputStream()->LA(1) != '-';
}

bool PostgreSQLLexerBase::CheckLaStar()
{
	return this->getInputStream()->LA(1) != '*';
}

bool PostgreSQLLexerBase::CharIsLetter()
{
	return true;
}

void PostgreSQLLexerBase::HandleNumericFail()
{
}

void PostgreSQLLexerBase::HandleLessLessGreaterGreater()
{
}

bool PostgreSQLLexerBase::CheckIfUtf32Letter()
{
	return true;
}


bool PostgreSQLLexerBase::IsSemiColon()
{
	return  ';' == (char)this->getInputStream()->LA(1);
}

