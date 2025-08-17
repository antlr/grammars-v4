package parser

import (
    "github.com/antlr4-go/antlr/v4"
)

type GvprParserBase struct {
    *antlr.BaseParser
}


func (p *GvprParserBase) IsSemiRequired() bool {
    x := p.GetTokenStream();
    c := x.LT(-1)
    return c.GetTokenType() != gvprParserCCBC
}

func (p *GvprParserBase) IsSemiNotRequired() bool {
    x := p.GetTokenStream();
    c := x.LT(-1)
    return c.GetTokenType() == gvprParserCCBC
}

