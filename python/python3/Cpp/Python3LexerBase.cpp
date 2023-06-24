#include "Python3Lexer.h"

using namespace antlr4;

void Python3LexerBase::emit(std::unique_ptr<antlr4::Token> t)
{
	tokens.push_back(cloneToken(t));
	setToken(std::move(t));
}

std::unique_ptr<antlr4::CommonToken> Python3LexerBase::cloneToken(const std::unique_ptr<antlr4::Token>& source) {
	return _factory->create({ this, _input }, source->getType(), source->getText(), source->getChannel(), source->getStartIndex(), source->getStopIndex(), source->getLine(), source->getCharPositionInLine());
}
