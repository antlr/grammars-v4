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
    la := stream.LA(1)
    return la == GoParserR_PAREN || la == GoParserR_CURLY || la == antlr.TokenEOF;
}

func (p *GoParserBase) isType() bool {
    stream := p.GetTokenStream()
    la := stream.LA(1)
    return la != GoParserIDENTIFIER;
}
