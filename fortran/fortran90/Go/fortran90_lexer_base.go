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
    var c1 = this.InputStream.LA(1);
    if (c1 == 'a') {
        var c2 = this.InputStream.LA(2);
        if (c2 == 'n') {
            var c3 = this.InputStream.LA(3);
            if (c3 == 'd') {
                var c4 = this.InputStream.LA(4);
                if (c4 == '.') {
                    return false;
                }
            }
        }
    }
    else if (c1 == 'o') {
        var c2 = this.InputStream.LA(2);
        if (c2 == 'r') {
            var c3 = this.InputStream.LA(3);
            if (c3 == '.') {
                return false;
            }
        }
        return true;
    }
}
