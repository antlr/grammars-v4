package parser

import (
    "github.com/antlr4-go/antlr/v4"
)

type Dart2LexerBase struct {
    *antlr.BaseLexer
}

func (l *Dart2LexerBase) CheckNotOpenBrace() bool {
    return l.GetInputStream().(antlr.CharStream).LA(1) != '{';
}
