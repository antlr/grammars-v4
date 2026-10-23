#include "awkLexerBase.h"
#include "awkLexer.h"

std::unique_ptr<antlr4::Token> awkLexerBase::nextToken()
{
    std::unique_ptr<antlr4::Token> next = antlr4::Lexer::nextToken();

    if (next->getChannel() == antlr4::Token::DEFAULT_CHANNEL) {
        // Keep track of the last token on the default channel.
	    this->_afterExpr = next->getType() == awkLexer::WORD
			      || next->getType() == awkLexer::NUMBER
			      || next->getType() == awkLexer::STRING
			      || next->getType() == awkLexer::BUILTIN_FUNC_NAME
			      || next->getType() == awkLexer::INCR
			      || next->getType() == awkLexer::DECR
			      || next->getType() == awkLexer::Rp
			      || next->getType() == awkLexer::Rb;
    }

    return next;
}

bool awkLexerBase::IsNotAfterExpr()
{
    return ! this->_afterExpr;
}
