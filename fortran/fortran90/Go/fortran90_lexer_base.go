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

func (l *Fortran90LexerBase) VerifyNotOperator() bool {
    c1 := l.GetInputStream().(antlr.CharStream).LA(1);
    if (c1 == 'a') {
        c2 := l.GetInputStream().(antlr.CharStream).LA(2);
        if (c2 == 'n') {
            c3 := l.GetInputStream().(antlr.CharStream).LA(3);
            if (c3 == 'd') {
                c4 := l.GetInputStream().(antlr.CharStream).LA(4);
                if (c4 == '.') {
                    return false;
                }
            }
        }
    } else if (c1 == 'o') {
        c2 := l.GetInputStream().(antlr.CharStream).LA(2);
        if (c2 == 'r') {
            c3 := l.GetInputStream().(antlr.CharStream).LA(3);
            if (c3 == '.') {
                return false;
            }
        }
    }
    return true;
}
