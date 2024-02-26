package parser

import (
	"github.com/antlr4-go/antlr/v4"
)

type BisonLexerBase struct {
	*antlr.BaseLexer
	percent_percent_count int
}

func (l *BisonLexerBase) NextMode() {
        l.percent_percent_count = l.percent_percent_count + 1;
        if (l.percent_percent_count == 1) {
            return;
        } else if (l.percent_percent_count == 2) {
            l.PushMode(BisonLexerEpilogueMode);
            return;
        } else {
            l.SetType(BisonLexerPercentPercent);
            return;
        }
}

func (l *BisonLexerBase) Reset() {
	l.percent_percent_count = 0
	l.BaseLexer.Reset()
}
