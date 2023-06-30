package parser

import "github.com/antlr4-go/antlr/v4"

type Python3LexerBase struct {
	*antlr.BaseLexer

	scopeStrictModes []bool
	stackLength      int
	stackIx          int

	lastToken        antlr.Token
	useStrictDefault bool
	useStrictCurrent bool
	templateDepth    int
}

func (l *Python3LexerBase) IsStartOfFile() bool {
	return l.lastToken == nil
}

func (l *Python3LexerBase) pushStrictModeScope(v bool) {
	if l.stackIx == l.stackLength {
		l.scopeStrictModes = append(l.scopeStrictModes, v)
		l.stackLength++
	} else {
		l.scopeStrictModes[l.stackIx] = v
	}
	l.stackIx++
}

func (l *Python3LexerBase) popStrictModeScope() bool {
	l.stackIx--
	v := l.scopeStrictModes[l.stackIx]
	l.scopeStrictModes[l.stackIx] = false
	return v
}

// IsStrictMode is self explanatory.
func (l *Python3LexerBase) IsStrictMode() bool {
	return l.useStrictCurrent
}

// NextToken from the character stream.
func (l *Python3LexerBase) NextToken() antlr.Token {
	next := l.BaseLexer.NextToken() // Get next token
	if next.GetChannel() == antlr.TokenDefaultChannel {
		// Keep track of the last token on default channel
		l.lastToken = next
	}
	return next
}

// ProcessOpenBrace is called when a { is encountered during
// lexing, we push a new scope everytime.
func (l *Python3LexerBase) ProcessOpenBrace() {
	l.useStrictCurrent = l.useStrictDefault
	if l.stackIx > 0 && l.scopeStrictModes[l.stackIx-1] {
		l.useStrictCurrent = true
	}
	l.pushStrictModeScope(l.useStrictCurrent)
}

// ProcessCloseBrace is called when a } is encountered during
// lexing, we pop a scope unless we're inside global scope.
func (l *Python3LexerBase) ProcessCloseBrace() {
	l.useStrictCurrent = l.useStrictDefault
	if l.stackIx > 0 {
		l.useStrictCurrent = l.popStrictModeScope()
	}
}

// ProcessStringLiteral is called when lexing a string literal.
func (l *Python3LexerBase) ProcessStringLiteral() {
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
func (l *Python3LexerBase) IsRegexPossible() bool {
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

func (l *Python3LexerBase) IncreaseTemplateDepth() {
	l.templateDepth++
}

func (l *Python3LexerBase) DecreaseTemplateDepth() {
	l.templateDepth--
}

func (l *Python3LexerBase) IsInTemplateString() bool {
	return l.templateDepth > 0
}

func (l *Python3LexerBase) Reset() {
    l.scopeStrictModes = nil
    l.stackLength = 0
    l.stackIx = 0
    l.lastToken = nil
    l.useStrictDefault = false
    l.useStrictCurrent = false
    l.templateDepth = 0
	l.BaseLexer.Reset()
}
