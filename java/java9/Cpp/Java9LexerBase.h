#pragma once
#include "antlr4-runtime.h"

class Java9LexerBase : public antlr4::Lexer
{
	private:
		antlr4::CharStream * _input;

	public:
		Java9LexerBase(antlr4::CharStream * input);

	class Character
	{
		public:
		    static bool isJavaIdentifierPart(int c);
		    static bool isJavaIdentifierStart(int c);
		    static int toCodePoint(int high, int low);
	};
		
	bool Check1();
	bool Check2();
	bool Check3();
	bool Check4();
};
