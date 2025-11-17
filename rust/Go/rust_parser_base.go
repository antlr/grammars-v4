package parser

import (
	"github.com/antlr4-go/antlr/v4"
)

type RustParserBase struct {
	*antlr.BaseParser
}

func (p *RustParserBase) NextGT() bool {
	return p.GetInputStream().LA(1) == RustParserGT
}

func (p *RustParserBase) NextLT() bool {
	return p.GetInputStream().LA(1) == RustParserLT
}
