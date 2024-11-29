import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:core';
import 'dart:convert';
import 'dart:collection';
import 'MySQLLexer.dart';
import 'MySQLParser.dart';

/** SQL modes that control parsing behavior. */
enum SqlMode {
  noMode,
  ansiQuotes,
  highNotPrecedence,
  pipesAsConcat,
  ignoreSpace,
  noBackslashEscapes,
}

/** Base parser class for MySQL parsing. */
abstract class MySQLParserBase extends Parser {

    // To parameterize the parsing process.
    int serverVersion = 0;
    Set<SqlMode> sqlModes = {};
    bool supportMle = true;

    MySQLParserBase(TokenStream input) : super(input)
	{
        this.serverVersion = 80200;
        this.sqlModeFromString("ANSI_QUOTES");
	}

    /// Determines if the given SQL mode is currently active.
    bool isSqlModeActive(SqlMode mode) {
        return sqlModes.contains(mode);
    }

    /// Checks if the parser is in pure identifier mode.
    bool isPureIdentifier() {
        return isSqlModeActive(SqlMode.ansiQuotes);
    }

    /// Checks if the parser is in text string literal mode.
    bool isTextStringLiteral() {
        return !isSqlModeActive(SqlMode.ansiQuotes);
    }

    /// Checks if the parser is handling a stored routine body.
    bool isStoredRoutineBody() {
        return serverVersion >= 80032 && supportMle;
    }

    /// Checks if the parser is handling a SELECT statement with INTO clause.
    bool isSelectStatementWithInto() {
        return serverVersion >= 80024 && serverVersion < 80031;
    }

    void sqlModeFromString(String modes) {
        sqlModes.clear();
        List<String> parts = modes.toUpperCase().split(',');

        for (String mode in parts) {
            if (['ANSI', 'DB2', 'MAXDB', 'MSSQL', 'ORACLE', 'POSTGRESQL']
                .contains(mode)) {
                sqlModes.add(SqlMode.ansiQuotes);
                sqlModes.add(SqlMode.pipesAsConcat);
                sqlModes.add(SqlMode.ignoreSpace);
            } else if (mode == 'ANSI_QUOTES') {
                sqlModes.add(SqlMode.ansiQuotes);
            } else if (mode == 'PIPES_AS_CONCAT') {
                sqlModes.add(SqlMode.pipesAsConcat);
            } else if (mode == 'NO_BACKSLASH_ESCAPES') {
                sqlModes.add(SqlMode.noBackslashEscapes);
            } else if (mode == 'IGNORE_SPACE') {
                sqlModes.add(SqlMode.ignoreSpace);
            } else if (['HIGH_NOT_PRECEDENCE', 'MYSQL323', 'MYSQL40']
                .contains(mode)) {
                sqlModes.add(SqlMode.highNotPrecedence);
            }
        }
    }
}
