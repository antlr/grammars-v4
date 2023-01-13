/*
 [The "BSD licence"]
 Copyright (c) 2005-2007 Terence Parr
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "LexerAdaptor.h"
#include "ANTLRv4Lexer.h"
#include "ctype.h"

//using namespace antlr4;

LexerAdaptor::LexerAdaptor(antlr4::CharStream *input) : Lexer(input) {
	currentRuleType = antlr4::Token::INVALID_TYPE;
	_insideOptionsBlock = false;
	PREQUEL_CONSTRUCT = -10;
	OPTIONS_CONSTRUCT = -11;
//	stream = input;
}

int LexerAdaptor::getCurrentRuleType() {
	return currentRuleType;
}

void LexerAdaptor::setCurrentRuleType(int ruleType) {
	currentRuleType = ruleType;
}

void LexerAdaptor::handleBeginArgument() {
	if (inLexerRule()) {
		pushMode(ANTLRv4Lexer::LexerCharSet);
		more();
	} else {
		pushMode(ANTLRv4Lexer::Argument);
	}
}

void LexerAdaptor::handleEndArgument() {
	popMode();
	if (modeStack.size() > 0) {
		setType(ANTLRv4Lexer::ARGUMENT_CONTENT);
	}
}

void LexerAdaptor::handleEndAction() {
	auto oldMode = mode;
	auto newMode = popMode();

	if (modeStack.size() > 0 && newMode == ANTLRv4Lexer::TargetLanguageAction && oldMode == newMode) {
		setType(ANTLRv4Lexer::ACTION_CONTENT);
	}
}

antlr4::Token* LexerAdaptor::emit() {
	if ((type == ANTLRv4Lexer::OPTIONS || type == ANTLRv4Lexer::TOKENS || type == ANTLRv4Lexer::CHANNELS) && currentRuleType == antlr4::Token::INVALID_TYPE)
	{
		// enter prequel construct ending with an RBRACE
		currentRuleType = PREQUEL_CONSTRUCT;
	}
	else if (type == ANTLRv4Lexer::OPTIONS && currentRuleType == ANTLRv4Lexer::TOKEN_REF)
	{
		currentRuleType = OPTIONS_CONSTRUCT;
	}
	else if (type == ANTLRv4Lexer::RBRACE && currentRuleType == PREQUEL_CONSTRUCT)
	{
		// exit prequel construct
		currentRuleType = antlr4::Token::INVALID_TYPE;
	}
	else if (type == ANTLRv4Lexer::RBRACE && currentRuleType == OPTIONS_CONSTRUCT)
	{ // exit options
		currentRuleType = ANTLRv4Lexer::TOKEN_REF;
	}
	else if (type == ANTLRv4Lexer::AT && currentRuleType == antlr4::Token::INVALID_TYPE)
	{
		// enter action
		currentRuleType = ANTLRv4Lexer::AT;
	}
	else if (type == ANTLRv4Lexer::SEMI && currentRuleType == OPTIONS_CONSTRUCT)
	{ // ';' in options { .... }. Don't change anything.
	}
	else if (type == ANTLRv4Lexer::END_ACTION && currentRuleType == ANTLRv4Lexer::AT)
	{
		// exit action
		currentRuleType = antlr4::Token::INVALID_TYPE;
	}
	else if (type == ANTLRv4Lexer::ID)
	{
		std::string firstChar = _input->getText(antlr4::misc::Interval(tokenStartCharIndex, tokenStartCharIndex));
		if (isupper(firstChar.at(0))) {
			type = ANTLRv4Lexer::TOKEN_REF;
		} else {
			type = ANTLRv4Lexer::RULE_REF;
		}

		if (currentRuleType == antlr4::Token::INVALID_TYPE) { // if outside of rule def
			currentRuleType = type; // set to inside lexer or parser rule
		}
	} else if (type == ANTLRv4Lexer::SEMI) { // exit rule def
		currentRuleType = antlr4::Token::INVALID_TYPE;
	}

	return Lexer::emit();
}

bool LexerAdaptor::inLexerRule() {
	return currentRuleType == ANTLRv4Lexer::TOKEN_REF;
}

bool LexerAdaptor::inParserRule() { // not used, but added for clarity
	return currentRuleType == ANTLRv4Lexer::RULE_REF;
}
