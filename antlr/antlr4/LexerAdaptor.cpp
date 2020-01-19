
#include "LexerAdaptor.h"
#include "ANTLRv4Lexer.h"
#include "ctype.h"

using namespace antlr4;

LexerAdaptor::LexerAdaptor(CharStream *input) : Lexer(input) {}

int
LexerAdaptor::getCurrentRuleType() {
	return currentRuleType;
}

void
LexerAdaptor::setCurrentRuleType(int ruleType) {
	currentRuleType = ruleType;
}

void
LexerAdaptor::handleBeginArgument() {
	if (inLexerRule()) {
		pushMode(ANTLRv4Lexer::LexerCharSet);
		more();
	} else {
		pushMode(ANTLRv4Lexer::Argument);
	}
}

void
LexerAdaptor::handleEndArgument() {
	popMode();
	if (modeStack.size() > 0) {
		setType(ANTLRv4Lexer::ARGUMENT_CONTENT);
	}
}

void
LexerAdaptor::handleEndAction() {
	popMode();
	if (modeStack.size() > 0) {
		setType(ANTLRv4Lexer::ACTION_CONTENT);
	}
}

Token*
LexerAdaptor::emit() {
	if (type == ANTLRv4Lexer::ID) {
		std::string firstChar = _input->getText(misc::Interval(tokenStartCharIndex, tokenStartCharIndex));
		if (isupper(firstChar.at(0))) {
			type = ANTLRv4Lexer::TOKEN_REF;
		} else {
			type = ANTLRv4Lexer::RULE_REF;
		}

		if (currentRuleType == Token::INVALID_TYPE) { // if outside of rule def
			currentRuleType = type; // set to inside lexer or parser rule
		}
	} else if (type == ANTLRv4Lexer::SEMI) { // exit rule def
		currentRuleType = Token::INVALID_TYPE;
	}

	return Lexer::emit();
}

bool
LexerAdaptor::inLexerRule() {
	return currentRuleType == ANTLRv4Lexer::TOKEN_REF;
}

bool
LexerAdaptor::inParserRule() { // not used, but added for clarity
	return currentRuleType == ANTLRv4Lexer::RULE_REF;
}

