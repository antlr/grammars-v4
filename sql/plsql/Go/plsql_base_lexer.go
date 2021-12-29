package parser

import "github.com/antlr/antlr4/runtime/Go/antlr"

// PlSqlBaseLexer state
type PlSqlBaseLexer struct {
    *antlr.BaseLexer

    lastToken antlr.Token
    self PlSqlBaseLexer
}

// NextToken from the character stream.
func (l *PlSqlBaseLexer) NextToken() antlr.Token {
    next := l.BaseLexer.NextToken() // Get next token
    if next.GetChannel() == antlr.TokenDefaultChannel {
        // Keep track of the last token on default channel
        l.lastToken = next
    }
    return next
}

// IsRegexPossible returns true if the lexer can match a
// regex literal.
func (l *PlSqlBaseLexer) IsNewlineAtPos(pos int) bool {
    if l.lastToken == nil {
        return true
    }
    la := b.input.LA(pos)
    return la == -1 || la == '\n'
}
