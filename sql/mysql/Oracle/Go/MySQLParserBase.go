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
    return StaticMySQLParserBase.serverVersion >= 80032 && StaticMySQLParserBase.supportMle
}

// isSelectStatementWithInto checks if the server version supports SELECT INTO syntax.
func (m *MySQLParserBase) isSelectStatementWithInto() bool {
    return StaticMySQLParserBase.serverVersion >= 80024 && StaticMySQLParserBase.serverVersion < 80031
}

func (m *MySQLParserBase) isServerVersionGe80004() bool { return StaticMySQLParserBase.serverVersion >= 80004 }
func (m *MySQLParserBase) isServerVersionGe80011() bool { return StaticMySQLParserBase.serverVersion >= 80011 }
func (m *MySQLParserBase) isServerVersionGe80013() bool { return StaticMySQLParserBase.serverVersion >= 80013 }
func (m *MySQLParserBase) isServerVersionGe80014() bool { return StaticMySQLParserBase.serverVersion >= 80014 }
func (m *MySQLParserBase) isServerVersionGe80016() bool { return StaticMySQLParserBase.serverVersion >= 80016 }
func (m *MySQLParserBase) isServerVersionGe80017() bool { return StaticMySQLParserBase.serverVersion >= 80017 }
func (m *MySQLParserBase) isServerVersionGe80018() bool { return StaticMySQLParserBase.serverVersion >= 80018 }
func (m *MySQLParserBase) isServerVersionGe80019() bool { return StaticMySQLParserBase.serverVersion >= 80019 }
func (m *MySQLParserBase) isServerVersionGe80024() bool { return StaticMySQLParserBase.serverVersion >= 80024 }
func (m *MySQLParserBase) isServerVersionGe80025() bool { return StaticMySQLParserBase.serverVersion >= 80025 }
func (m *MySQLParserBase) isServerVersionGe80027() bool { return StaticMySQLParserBase.serverVersion >= 80027 }
func (m *MySQLParserBase) isServerVersionGe80031() bool { return StaticMySQLParserBase.serverVersion >= 80031 }
func (m *MySQLParserBase) isServerVersionGe80032() bool { return StaticMySQLParserBase.serverVersion >= 80032 }
func (m *MySQLParserBase) isServerVersionGe80100() bool { return StaticMySQLParserBase.serverVersion >= 80100 }
func (m *MySQLParserBase) isServerVersionGe80200() bool { return StaticMySQLParserBase.serverVersion >= 80200 }
func (m *MySQLParserBase) isServerVersionLt80011() bool { return StaticMySQLParserBase.serverVersion < 80011 }
func (m *MySQLParserBase) isServerVersionLt80012() bool { return StaticMySQLParserBase.serverVersion < 80012 }
func (m *MySQLParserBase) isServerVersionLt80014() bool { return StaticMySQLParserBase.serverVersion < 80014 }
func (m *MySQLParserBase) isServerVersionLt80016() bool { return StaticMySQLParserBase.serverVersion < 80016 }
func (m *MySQLParserBase) isServerVersionLt80017() bool { return StaticMySQLParserBase.serverVersion < 80017 }
func (m *MySQLParserBase) isServerVersionLt80024() bool { return StaticMySQLParserBase.serverVersion < 80024 }
func (m *MySQLParserBase) isServerVersionLt80025() bool { return StaticMySQLParserBase.serverVersion < 80025 }
func (m *MySQLParserBase) isServerVersionLt80031() bool { return StaticMySQLParserBase.serverVersion < 80031 }
