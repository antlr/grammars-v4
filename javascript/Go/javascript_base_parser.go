package parser

import (
	"strings"

	"github.com/antlr/antlr4/runtime/Go/antlr"
)

// JavaScriptBaseParser implementation.
type JavaScriptBaseParser struct {
	*antlr.BaseParser
}

// Short for p.prev(str string)
func (p *JavaScriptBaseParser) p(str string) bool {
	return p.prev(str)
}

// Whether the previous token value equals to str.
func (p *JavaScriptBaseParser) prev(str string) bool {
	return p.GetTokenStream().LT(-1).GetText() == str
}

// Short for p.next(str string)
func (p *JavaScriptBaseParser) n(str string) bool {
	return p.next(str)
}

// Whether the next token value equals to str.
func (p *JavaScriptBaseParser) next(str string) bool {
	return p.GetTokenStream().LT(1).GetText() == str
}

func (p *JavaScriptBaseParser) notLineTerminator() bool {
	return !p.here(JavaScriptParserLineTerminator)
}

func (p *JavaScriptBaseParser) notOpenBraceAndNotFunction() bool {
	nextTokenType := p.GetTokenStream().LT(1).GetTokenType()
	return nextTokenType != JavaScriptParserOpenBrace && nextTokenType != JavaScriptParserFunction
}

func (p *JavaScriptBaseParser) closeBrace() bool {
	return p.GetTokenStream().LT(1).GetTokenType() == JavaScriptParserCloseBrace
}

// Returns true if on the current index of the parser's
// token stream a token of the given type exists on the
// Hidden channel.
func (p *JavaScriptBaseParser) here(_type int) bool {
	// Get the token ahead of the current index.
	possibleIndexEosToken := p.GetCurrentToken().GetTokenIndex() - 1
	ahead := p.GetTokenStream().Get(possibleIndexEosToken)

	// Check if the token resides on the HIDDEN channel and if it's of the
	// provided type.
	return ahead.GetChannel() == antlr.LexerHidden && ahead.GetTokenType() == _type
}

// Returns true if on the current index of the parser's
// token stream a token exists on the Hidden channel which
// either is a line terminator, or is a multi line comment that
// contains a line terminator.
func (p *JavaScriptBaseParser) lineTerminatorAhead() bool {
	// Get the token ahead of the current index.
	possibleIndexEosToken := p.GetCurrentToken().GetTokenIndex() - 1
	ahead := p.GetTokenStream().Get(possibleIndexEosToken)

	if ahead.GetChannel() != antlr.LexerHidden {
		// We're only interested in tokens on the HIDDEN channel.
		return true
	}

	if ahead.GetTokenType() == JavaScriptParserLineTerminator {
		// There is definitely a line terminator ahead.
		return true
	}

	if ahead.GetTokenType() == JavaScriptParserWhiteSpaces {
		// Get the token ahead of the current whitespaces.
		possibleIndexEosToken = p.GetCurrentToken().GetTokenIndex() - 2
		ahead = p.GetTokenStream().Get(possibleIndexEosToken)
	}

	// Get the token's text and type.
	text := ahead.GetText()
	_type := ahead.GetTokenType()

	// Check if the token is, or contains a line terminator.
	return (_type == JavaScriptParserMultiLineComment && (strings.Contains(text, "\r") || strings.Contains(text, "\n"))) ||
		(_type == JavaScriptParserLineTerminator)
}
