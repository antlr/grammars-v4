package parser

import (
    "github.com/antlr/antlr4/runtime/Go/antlr"
)

// PlSqlBaseParser implementation.
type PlSqlBaseParser struct {
    *antlr.BaseParser
    _isVersion12 bool
    _isVersion10 bool
    self PlSqlParserBase
}

/*
PlSqlParserBase(TokenStream input) {
    super(input);
    self = this;
}

func isVersion12() bool {
    return _isVersion12;
}

func setVersion12(boolean value) {
    _isVersion12 = value;
}

func isVersion10() func {
    return _isVersion10;
}

func setVersion10(boolean value) {
_isVersion10 = value;
}
*/