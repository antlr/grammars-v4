package parser

import (
	"github.com/antlr4-go/antlr/v4"
)

// RustParserBase implementation.
type RustParserBase struct {
	*antlr.BaseParser
}

func (p *RustParserBase) next(expect int) bool {
	return p.GetInputStream().LA(1) == expect
}
