package parser

import (
    "github.com/antlr4-go/antlr/v4"
)

// PlSqlParserBase implementation.
type PlSqlParserBase struct {
    *antlr.BaseParser
    _isVersion12 bool
    _isVersion11 bool
    _isVersion10 bool
}

var StaticConfig PlSqlParserBase

func init() {
    StaticConfig = PlSqlParserBase {
        _isVersion12: true,
        _isVersion11: true,
	    _isVersion10: true,
    }
}

func (p *PlSqlParserBase) isVersion12() bool {
    return StaticConfig._isVersion12;
}

func (p *PlSqlParserBase) setVersion12(value bool) {
    StaticConfig._isVersion12 = value;
}

func (p *PlSqlParserBase) isVersion11() bool {
    return StaticConfig._isVersion11;
}

func (p *PlSqlParserBase) setVersion11(value bool) {
    StaticConfig._isVersion11 = value;
}

func (p *PlSqlParserBase) isVersion10() bool {
    return StaticConfig._isVersion10;
}

func (p *PlSqlParserBase) setVersion10(value bool) {
    StaticConfig._isVersion10 = value;
}
