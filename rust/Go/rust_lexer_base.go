package parser

import (
	"github.com/antlr4-go/antlr/v4"
)

type RustLexerBase struct {
	*antlr.BaseLexer

	lastToken  antlr.Token
	lastToken2 antlr.Token
}

func (l *RustLexerBase) NextToken() antlr.Token {
	next := l.BaseLexer.NextToken()

	if next.GetChannel() == antlr.LexerDefaultTokenChannel {
		// keep track of the last token on the default channel
		l.lastToken2 = l.lastToken
		l.lastToken = next
	}

	return next
}

func (l *RustLexerBase) SOF() bool {
	return l.GetInputStream().LA(-1) <= 0
}

func (l *RustLexerBase) floatDotPossible() bool {
	next := rune(l.GetInputStream().LA(1))
	if next == '.' || next == '_' {
		return false
	}
	if next == 'f' {
		// 1.f32
		if rune(l.GetInputStream().LA(2)) == '3' && rune(l.GetInputStream().LA(3)) == '2' {
			return true
		}
		//1.f64
		if rune(l.GetInputStream().LA(2)) == '6' && rune(l.GetInputStream().LA(3)) == '4' {
			return true
		}
		return false
	}
	if next >= 'a' && next <= 'z' {
		return false
	}
	if next >= 'A' && next <= 'Z' {
		return false
	}

	return false
}

func (l *RustLexerBase) floatLiteralPossible() bool {
	if l.lastToken == nil || l.lastToken2 == nil {
		return true
	}
	if l.lastToken.GetTokenType() != RustLexerDOT {
		return true
	}

	switch l.lastToken2.GetTokenType() {
	case RustLexerCHAR_LITERAL, RustLexerSTRING_LITERAL,
		RustLexerRAW_STRING_LITERAL, RustLexerBYTE_LITERAL,
		RustLexerBYTE_STRING_LITERAL, RustLexerRAW_BYTE_STRING_LITERAL,
		RustLexerINTEGER_LITERAL, RustLexerDEC_LITERAL, RustLexerHEX_LITERAL,
		RustLexerOCT_LITERAL, RustLexerBIN_LITERAL, RustLexerKW_SUPER,
		RustLexerKW_SELFVALUE, RustLexerKW_SELFTYPE, RustLexerKW_CRATE,
		RustLexerKW_DOLLARCRATE, RustLexerGT, RustLexerRCURLYBRACE,
		RustLexerRSQUAREBRACKET, RustLexerRPAREN, RustLexerKW_AWAIT,
		RustLexerNON_KEYWORD_IDENTIFIER, RustLexerRAW_IDENTIFIER,
		RustLexerKW_MACRORULES:
		return false
	default:
		return true
	}

	return false
}
