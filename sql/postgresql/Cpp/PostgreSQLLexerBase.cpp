#include "antlr4-runtime.h"
#include "PostgreSQLLexerBase.h"
#include "PostgreSQLLexer.h"
#include <locale>
#include <codecvt>
#include <cwctype>

PostgreSQLLexerBase::PostgreSQLLexerBase(antlr4::CharStream * input) : antlr4::Lexer(input)
{
    _input = input;
}

void PostgreSQLLexerBase::PushTag()
{
	tags.push(this->getText());
}

bool PostgreSQLLexerBase::IsTag()
{
	return this->getText() == tags.top();
}

void PostgreSQLLexerBase::PopTag()
{
	tags.pop();
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
	return std::iswalpha(static_cast<char>(this->getInputStream()->LA(-1)));
}

void PostgreSQLLexerBase::HandleNumericFail()
{
	this->getInputStream()->seek(this->getInputStream()->index() - 2);
	this->setType(PostgreSQLLexer::Integral);
}

void PostgreSQLLexerBase::HandleLessLessGreaterGreater()
{
	if (this->getText() == "<<") this->setType(PostgreSQLLexer::LESS_LESS);
	if (this->getText() == ">>") this->setType(PostgreSQLLexer::GREATER_GREATER);
}


char32_t surrogate_to_utf32(char16_t high, char16_t low)
{ 
	return (high << 10) + low - 0x35fdc00; 
}

int toCodePoint(int high, int low)
{
	return surrogate_to_utf32(high, low);
}


bool PostgreSQLLexerBase::CheckIfUtf32Letter()
{
	char high = static_cast<char>(this->getInputStream()->LA(-2));
	char low = static_cast<char>(this->getInputStream()->LA(-1));
	return std::iswalpha(toCodePoint(high, low));
}

bool PostgreSQLLexerBase::IsSemiColon()
{
	return  ';' == (char)this->getInputStream()->LA(1);
}
