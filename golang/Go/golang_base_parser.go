package parser

import (
	"strings"

	"github.com/antlr/antlr4/runtime/Go/antlr"
)

// GoBaseParser implementation.
type GoBaseParser struct {
	*antlr.BaseParser
}

// Returns true if on the current index of the parser's
// token stream a token exists on the Hidden channel which
// either is a line terminator, or is a multi line comment that
// contains a line terminator.
func (p *GoBaseParser) lineTerminatorAhead() bool {
	// Get the token ahead of the current index.
	possibleIndexEosToken := p.GetCurrentToken().GetTokenIndex() - 1
	ahead := p.GetTokenStream().Get(possibleIndexEosToken)

	if ahead.GetChannel() != antlr.LexerHidden {
		// We're only interested in tokens on the HIDDEN channel.
		return true
	}

	if ahead.GetTokenType() == GolangParserTERMINATOR {
		// There is definitely a line terminator ahead.
		return true
	}

	if ahead.GetTokenType() == GolangParserWS {
		// Get the token ahead of the current whitespaces.
		possibleIndexEosToken = p.GetCurrentToken().GetTokenIndex() - 2
		ahead = p.GetTokenStream().Get(possibleIndexEosToken)
	}

	// Get the token's text and type.
	text := ahead.GetText()
	_type := ahead.GetTokenType()

	// Check if the token is, or contains a line terminator.
	return (_type == GolangParserCOMMENT && (strings.Contains(text, "\r") || strings.Contains(text, "\n"))) ||
		(_type == GolangParserTERMINATOR)
}

func (p *GoBaseParser) noTerminatorBetween(tokenOffset int) bool {
	var s antlr.TokenStream
	s = p.GetTokenStream()
	stream := s.(*antlr.CommonTokenStream)
	tokens := stream.GetHiddenTokensToLeft(stream.LT(tokenOffset).GetTokenIndex(), -1)
	if tokens == nil {
		return true
	}

	for _, token := range tokens {
		if strings.Contains(token.GetText(), "\n") {
			return false
		}
	}
	return true
}

func (p *GoBaseParser) noTerminatorAfterParams(tokenOffset int) bool {
	stream := p.GetTokenStream()
	leftParams := 1
	rightParams := 0
	var value string

	if stream.LT(tokenOffset).GetText() == "(" {
		for leftParams != rightParams {
			tokenOffset++
			value = stream.LT(tokenOffset).GetText()
			if value == "(" {
				leftParams++
			} else if value == ")" {
				rightParams++
			}
		}
		tokenOffset++
		return p.noTerminatorBetween(tokenOffset)
	}
	return true
}

func (p *GoBaseParser) closeBrace() bool {
	stream := p.GetTokenStream()
	return stream.LT(1).GetText() == "}"
}
