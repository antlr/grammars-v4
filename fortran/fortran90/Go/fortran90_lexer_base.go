package parser

import (
    "github.com/antlr4-go/antlr/v4"
)

type Fortran90LexerBase struct {
    *antlr.BaseLexer
    lastToken antlr.Token
}

func (l *Fortran90LexerBase) IsColumnZero() bool {
    return l.GetCharPositionInLine() == 0;
}
