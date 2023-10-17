package parser

import (
	"strings"

	"github.com/antlr4-go/antlr/v4"
)

// JavaScriptParserBase implementation.
type JavaScriptParserBase struct {
	*antlr.BaseParser
}

// Short for p.prev(str string)
func (p *JavaScriptParserBase) p(str string) bool {
	return p.prev(str)
}

// Whether the previous token value equals to str.
func (p *JavaScriptParserBase) prev(str string) bool {
	return p.GetTokenStream().LT(-1).GetText() == str
}

// Short for p.next(str string)
func (p *JavaScriptParserBase) n(str string) bool {
	return p.next(str)
}

// Whether the next token value equals to str.
func (p *JavaScriptParserBase) next(str string) bool {
	return p.GetTokenStream().LT(1).GetText() == str
}

func (p *JavaScriptParserBase) notLineTerminator() bool {
	return !p.here(JavaScriptParserLineTerminator)
}

func (p *JavaScriptParserBase) notOpenBraceAndNotFunction() bool {
	nextTokenType := p.GetTokenStream().LT(1).GetTokenType()
	return nextTokenType != JavaScriptParserOpenBrace && nextTokenType != JavaScriptParserFunction_
}

func (p *JavaScriptParserBase) closeBrace() bool {
	return p.GetTokenStream().LT(1).GetTokenType() == JavaScriptParserCloseBrace
}

// Returns true if on the current index of the parser's
// token stream a token of the given type exists on the
// Hidden channel.
func (p *JavaScriptParserBase) here(_type int) bool {
	// Get the most recently emitted token.
	currentToken := p.GetTokenStream().LT(-1)

	// Get the next token index.
	nextTokenIndex := 0

	if currentToken != nil {
		nextTokenIndex = currentToken.GetTokenIndex() + 1
	}

	// Get the token after the `currentToken`. By using `_input.get(index)`,
	// we also grab a token that is (possibly) on the HIDDEN channel.
	nextToken := p.GetTokenStream().Get(nextTokenIndex);

	// Check if the token resides on the HIDDEN channel and if it's of the
	// provided type.
	return nextToken.GetChannel() == antlr.LexerHidden && nextToken.GetTokenType() == _type
}

// Returns true if on the current index of the parser's
// token stream a token exists on the Hidden channel which
// either is a line terminator, or is a multi line comment that
// contains a line terminator.
func (p *JavaScriptParserBase) lineTerminatorAhead() bool {
	// Get the token ahead of the current index.
	possibleIndexEosToken := p.GetCurrentToken().GetTokenIndex() - 1
        if possibleIndexEosToken < 0 {
            return false
        }
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
