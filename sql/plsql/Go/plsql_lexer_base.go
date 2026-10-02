package parser

import (
    "github.com/antlr4-go/antlr/v4"
)

// PlSqlLexerBase state
type PlSqlLexerBase struct {
    *antlr.BaseLexer
}

func (l *PlSqlLexerBase) IsNewlineAtPos(pos int) bool {
    la := l.GetInputStream().LA(pos)
    return la == -1 || la == '\n'
}

func (l *PlSqlLexerBase) IsQQuoteDelimiter() bool {
    input := l.GetInputStream()
    opening := input.LA(l.TokenStartCharIndex + 2 - l.GetCharIndex())
    closing := input.LA(-2)
    switch opening {
    case '[':
        return closing == ']'
    case '{':
        return closing == '}'
    case '<':
        return closing == '>'
    case '(':
        return closing == ')'
    default:
        return closing == opening
    }
}
