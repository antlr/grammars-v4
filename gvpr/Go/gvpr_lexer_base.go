package parser

import (
    "github.com/antlr4-go/antlr/v4"
)

type GvprLexerBase struct {
    *antlr.BaseLexer
}

func (l *GvprLexerBase) IsColumnZero() bool {
    return l.GetCharPositionInLine() == 1
}
