package parser

import "github.com/antlr/antlr4/runtime/Go/antlr"

// PlSqlBaseLexer state
type PlSqlLexerBase struct {
    *antlr.BaseLexer

    lastToken antlr.Token
}

// NextToken from the character stream.
func (p *PlSqlLexerBase) NextToken() antlr.Token {
    next := p.BaseLexer.NextToken() // Get next token
    if next.GetChannel() == antlr.TokenDefaultChannel {
        // Keep track of the last token on default channel
        p.lastToken = next
    }
    return next
}

// IsRegexPossible returns true if the lexer can match a
// regex literal.
func (p *PlSqlLexerBase) IsNewlineAtPos(pos int) bool {
    if p.lastToken == nil {
        return true
    }
    la := p.GetInputStream().LA(pos)
    return la == -1 || la == '\n'
}
