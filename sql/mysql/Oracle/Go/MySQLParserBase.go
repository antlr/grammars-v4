package parser

import (
	"github.com/antlr4-go/antlr/v4"
)

type MySQLParserBase struct {
    *antlr.BaseParser
    serverVersion int
    sqlModes           map[SqlMode]bool
    supportMle bool
}

var StaticMySQLParserBase MySQLParserBase

func init() {
    StaticMySQLParserBase = MySQLParserBase {
        supportMle:     true,
        serverVersion: 80200,
    }
    StaticMySQLParserBase.sqlModes = sqlModeFromString("ANSI_QUOTES");
}

func NewMySQLParserBase(input antlr.InputStream) *MySQLParserBase {
    r := &MySQLParserBase{
        supportMle:     true,
        serverVersion: 80200,
    }
    r.sqlModes = make(map[SqlMode]bool)
    return r
}

// isSqlModeActive determines if the given SQL mode is currently active in the lexer.
func (m *MySQLParserBase) isSqlModeActive(mode SqlMode) bool {
    return StaticMySQLParserBase.sqlModes[mode]
}

// isPureIdentifier checks if the lexer is in ANSI_QUOTES mode.
func (m *MySQLParserBase) isPureIdentifier() bool {
    return m.isSqlModeActive(AnsiQuotes)
}

// isTextStringLiteral checks if the lexer is not in ANSI_QUOTES mode.
func (m *MySQLParserBase) isTextStringLiteral() bool {
    return !m.isSqlModeActive(AnsiQuotes)
}

// isStoredRoutineBody checks if the server version supports stored routine body.
func (m *MySQLParserBase) isStoredRoutineBody() bool {
    return m.serverVersion >= 80032 && m.supportMle
}

// isSelectStatementWithInto checks if the server version supports SELECT INTO syntax.
func (m *MySQLParserBase) isSelectStatementWithInto() bool {
    return m.serverVersion >= 80024 && m.serverVersion < 80031
}

