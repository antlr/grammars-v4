package parser

import (
	"github.com/antlr4-go/antlr/v4"
)

// GoParserBase implementation.
type GoParserBase struct {
	*antlr.BaseParser
}


// Returns true if the current Token is a closing bracket (")" or "}")
func (p *GoParserBase) closingBracket() bool {
	stream := p.GetTokenStream()
	prevTokenType := stream.LA(1)
	return prevTokenType == GoParserR_PAREN || prevTokenType == GoParserR_CURLY;
}
