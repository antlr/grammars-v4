package parser

import "github.com/antlr/antlr4/runtime/Go/antlr"

// JavaScriptBaseLexer state
type JavaScriptBaseLexer struct {
	*antlr.BaseLexer

	scopeStrictModes []bool
	stackLength      int
	stackIx          int

	lastToken        antlr.Token
	useStrictDefault bool
	useStrictCurrent bool
}

func (l *JavaScriptBaseLexer) pushStrictModeScope(v bool) {
	if l.stackIx == l.stackLength {
		l.scopeStrictModes = append(l.scopeStrictModes, v)
		l.stackLength++
	} else {
		l.scopeStrictModes[l.stackIx] = v
	}
	l.stackIx++
}

func (l *JavaScriptBaseLexer) popStrictModeScope() bool {
	l.stackIx--
	v := l.scopeStrictModes[l.stackIx]
	l.scopeStrictModes[l.stackIx] = false
	return v
}

// IsStrictMode is self explanatory.
func (l *JavaScriptBaseLexer) IsStrictMode() bool {
	return l.useStrictCurrent
}

// NextToken from the character stream.
func (l *JavaScriptBaseLexer) NextToken() antlr.Token {
	next := l.BaseLexer.NextToken() // Get next token
	if next.GetChannel() == antlr.TokenDefaultChannel {
		// Keep track of the last token on default channel
		l.lastToken = next
	}
	return next
}

// ProcessOpenBrace is called when a { is encountered during
// lexing, we push a new scope everytime.
func (l *JavaScriptBaseLexer) ProcessOpenBrace() {
	l.useStrictCurrent = l.useStrictDefault
	if l.stackIx > 0 && l.scopeStrictModes[l.stackIx-1] {
		l.useStrictCurrent = true
	}
	l.pushStrictModeScope(l.useStrictCurrent)
}

// ProcessCloseBrace is called when a } is encountered during
// lexing, we pop a scope unless we're inside global scope.
func (l *JavaScriptBaseLexer) ProcessCloseBrace() {
	l.useStrictCurrent = l.useStrictDefault
	if l.stackIx > 0 {
		l.useStrictCurrent = l.popStrictModeScope()
	}
}

// ProcessStringLiteral is called when lexing a string literal.
func (l *JavaScriptBaseLexer) ProcessStringLiteral() {
	if l.lastToken == nil || l.lastToken.GetTokenType() == JavaScriptLexerOpenBrace {
		if l.GetText() == `"use strict"` || l.GetText() == "'use strict'" {
			if l.stackIx > 0 {
				l.popStrictModeScope()
			}
			l.useStrictCurrent = true
			l.pushStrictModeScope(l.useStrictCurrent)
		}
	}
}

// IsRegexPossible returns true if the lexer can match a
// regex literal.
func (l *JavaScriptBaseLexer) IsRegexPossible() bool {
	if l.lastToken == nil {
		return true
	}
	switch l.lastToken.GetTokenType() {
	case JavaScriptLexerIdentifier, JavaScriptLexerNullLiteral,
		JavaScriptLexerBooleanLiteral, JavaScriptLexerThis,
		JavaScriptLexerCloseBracket, JavaScriptLexerCloseParen,
		JavaScriptLexerOctalIntegerLiteral, JavaScriptLexerDecimalLiteral,
		JavaScriptLexerHexIntegerLiteral, JavaScriptLexerStringLiteral,
		JavaScriptLexerPlusPlus, JavaScriptLexerMinusMinus:
		return false
	default:
		return true
	}
}
