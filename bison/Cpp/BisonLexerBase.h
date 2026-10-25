#pragma once
#include "antlr4-runtime.h"

class BisonLexerBase : public antlr4::Lexer
{
	private:
		antlr4::CharStream * _input;
		int percent_percent_count;

	public:
		BisonLexerBase(antlr4::CharStream * input);
		void NextMode();
		virtual void reset() override;
};
