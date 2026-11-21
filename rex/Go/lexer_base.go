package parser

import "github.com/antlr4-go/antlr/v4"

type LexerBase struct {
	*antlr.BaseLexer
}

func (l *LexerBase) Check1() bool {
	return l.GetInputStream().LA(1) != ':'
}
