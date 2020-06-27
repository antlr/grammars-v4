package parser

import "github.com/antlr/antlr4/runtime/Go/antlr"

// TJSBaseLexer state
type TJSBaseLexer struct {
	*antlr.BaseLexer

	lastToken        antlr.Token
}

// NextToken from the character stream.
func (l *TJSBaseLexer) NextToken() antlr.Token {
	next := l.BaseLexer.NextToken() // Get next token
	if next.GetChannel() == antlr.TokenDefaultChannel {
		// Keep track of the last token on default channel
		l.lastToken = next
	}
	return next
}

// IsRegexPossible returns true if the lexer can match a
// regex literal.
func (l *TJSBaseLexer) IsRegexPossible() bool {
	if l.lastToken == nil {
		return true
	}
	switch l.lastToken.GetTokenType() {
	case TJSLexerIdentifier, TJSLexerNullLiteral,
		TJSLexerBooleanLiteral, TJSLexerThis,
		TJSLexerCloseBracket, TJSLexerCloseParen,
		TJSLexerOctalIntegerLiteral, TJSLexerDecimalLiteral,
		TJSLexerHexIntegerLiteral, TJSLexerStringLiteral,
		TJSLexerPlusPlus, TJSLexerMinusMinus:
		return false
	default:
		return true
	}
}