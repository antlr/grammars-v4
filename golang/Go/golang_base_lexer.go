package parser

import "github.com/antlr/antlr4/runtime/Go/antlr"

// GoBaseLexer implementation
type GoBaseLexer struct {
	*antlr.BaseLexer
	lastToken antlr.Token
}

func (l *GoBaseLexer) nextToken() antlr.Token {
	next := l.BaseLexer.NextToken()
	if next.GetChannel() == antlr.TokenDefaultChannel {
		l.lastToken = next
	}
	return next
}
