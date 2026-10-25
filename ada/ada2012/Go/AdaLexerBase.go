package parser

import (
	"github.com/antlr4-go/antlr/v4"
)

type AdaLexerBase struct {
	*antlr.BaseLexer
	_lastTokenType int
}

func (l *AdaLexerBase) NextToken() antlr.Token {
	token := l.BaseLexer.NextToken()
	if token.GetChannel() == antlr.TokenDefaultChannel {
		l._lastTokenType = token.GetTokenType()
	}
	return token
}

func (l *AdaLexerBase) IsCharLiteralAllowed() bool {
	// In Ada, a tick after an identifier, closing paren, or 'all' keyword
	// is an attribute tick, not the start of a character literal.
	return l._lastTokenType != AdaLexerIDENTIFIER_ &&
		l._lastTokenType != AdaLexerRP &&
		l._lastTokenType != AdaLexerALL
}
