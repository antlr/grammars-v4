package parser

import (
    "github.com/antlr4-go/antlr/v4"
)

type LuaParserBase struct {
    *antlr.BaseParser
}

func (p *LuaParserBase) IsFunctionCall() bool {
    stream := p.GetTokenStream()
    la := stream.LT(1)
    if la.GetTokenType() != LuaParserNAME {
	return false
    }
    la = stream.LT(2)
    if la.GetTokenType() == LuaParserOP {
	return false
    }
    return true
}
