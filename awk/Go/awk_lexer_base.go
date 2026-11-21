package parser

import "github.com/antlr4-go/antlr/v4"

type awkLexerBase struct {
	*antlr.BaseLexer
	_afterExpr bool
}

func (l *awkLexerBase) NextToken() antlr.Token {
	token := l.BaseLexer.NextToken()
	if token.GetChannel() == antlr.TokenDefaultChannel {
		t := token.GetTokenType()
		l._afterExpr = t == awkLexerWORD ||
			t == awkLexerNUMBER ||
			t == awkLexerSTRING ||
			t == awkLexerBUILTIN_FUNC_NAME ||
			t == awkLexerINCR ||
			t == awkLexerDECR ||
			t == awkLexerRp ||
			t == awkLexerRb
	}
	return token
}

func (l *awkLexerBase) IsNotAfterExpr() bool {
	return !l._afterExpr
}
