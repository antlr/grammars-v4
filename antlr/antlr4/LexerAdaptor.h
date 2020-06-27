#pragma once


#include "antlr4-runtime.h"

class LexerAdaptor : public antlr4::Lexer {

	public:
	LexerAdaptor(antlr4::CharStream *input);
	int currentRuleType = antlr4::Token::INVALID_TYPE;

	int getCurrentRuleType();

	void setCurrentRuleType(int ruleType);

	void handleBeginArgument();

	void handleEndArgument();

	void handleEndAction();

	antlr4::Token* emit();

	bool inLexerRule();

	bool inParserRule();
};
