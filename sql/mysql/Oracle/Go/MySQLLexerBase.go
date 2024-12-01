package parser

import (
	"regexp"
	"github.com/antlr4-go/antlr/v4"
)

type MySQLLexerBase struct {
	*antlr.BaseLexer
        serverVersion int
}

