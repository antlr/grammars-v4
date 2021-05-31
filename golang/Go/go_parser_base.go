package parser

import (
	"strings"

	"github.com/antlr/antlr4/runtime/Go/antlr"
)

// GoParserBase implementation.
type GoParserBase struct {
	*antlr.BaseParser
}

// Returns true if on the current index of the parser's
// token stream a token exists on the Hidden channel which
// either is a line terminator, or is a multi line comment that
// contains a line terminator.
func (p *GoParserBase) lineTerminatorAhead() bool {
	// Get the token ahead of the current index.
	offset := 1
	possibleIndexEosToken := p.GetCurrentToken().GetTokenIndex() - offset

	if possibleIndexEosToken == -1 {
		return true
	}

	ahead := p.GetTokenStream().Get(possibleIndexEosToken)

	for ahead.GetChannel() == antlr.LexerHidden {
		if ahead.GetTokenType() == GoLexerTERMINATOR {
			return true
		}
		if ahead.GetTokenType() == GoLexerWS {
			offset++
			possibleIndexEosToken = p.GetCurrentToken().GetTokenIndex() - offset
			ahead = p.GetTokenStream().Get(possibleIndexEosToken)
		}
		if ahead.GetTokenType() == GoLexerCOMMENT || ahead.GetTokenType() == GoLexerLINE_COMMENT {
			if strings.Contains(ahead.GetText(), "\r") || strings.Contains(ahead.GetText(), "\n") {
				return true
			} else {
				offset++
				possibleIndexEosToken = p.GetCurrentToken().GetTokenIndex() - offset
				ahead = p.GetTokenStream().Get(possibleIndexEosToken)
			}
		}
	}

	return false
}

func (p *GoParserBase) noTerminatorBetween(tokenOffset int) bool {
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

func (p *GoParserBase) noTerminatorAfterParams(tokenOffset int) bool {
	stream := p.GetTokenStream()
	leftParams := 1
	rightParams := 0
	var tokenType int

	if stream.LT(tokenOffset).GetTokenType() == GoLexerL_PAREN {
		for leftParams != rightParams {
			tokenOffset++
			tokenType = stream.LT(tokenOffset).GetTokenType()
			if tokenType == GoLexerL_PAREN {
				leftParams++
			} else if tokenType == GoLexerR_PAREN {
				rightParams++
			}
		}
		tokenOffset++
		return p.noTerminatorBetween(tokenOffset)
	}
	return true
}

func (p *GoParserBase) checkPreviousTokenText(text string) bool {
	stream := p.GetTokenStream()
	return stream.LT(1).GetText() == text
}
