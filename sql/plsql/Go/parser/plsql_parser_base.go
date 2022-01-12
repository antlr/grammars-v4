package parser

import (
    "github.com/antlr/antlr4/runtime/Go/antlr"
)

// PlSqlParserBase implementation.
type PlSqlParserBase struct {
    *antlr.BaseParser
    _isVersion12 bool
    _isVersion10 bool
}

//func NewPlSqlParserBase(input TokenStream) *SqlParserBase {
//	b := antlr.NewBaseParser(input)
//	return &PlSqlParserBase{antlr.BaseParser: b, _isVersion12: true, _isVersion10: false}
//}

func (p *PlSqlParserBase) isVersion12() bool {
    return p._isVersion12
}

func (p *PlSqlParserBase) SetVersion12(value bool) {
    p._isVersion12 = value
}

func (p *PlSqlParserBase) isVersion10() bool {
    return p._isVersion10
}

func (p *PlSqlParserBase) SetVersion10(value bool) {
    p._isVersion10 = value
}
