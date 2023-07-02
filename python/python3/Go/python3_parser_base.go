package parser

import (
	"github.com/antlr4-go/antlr/v4"
)

type Python3ParserBase struct {
	*antlr.BaseParser
}

func (p *Python3ParserBase) CannotBePlusMinus() bool {
	return true
}

func (p *Python3ParserBase) CannotBeDotLpEq() bool {
	return true
}









