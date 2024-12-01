import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:core';
import 'dart:convert';
import 'dart:collection';
import 'MySQLLexer.dart';
import 'MySQLParser.dart';
import 'SqlMode.dart';
import 'SqlModes.dart';

/** Base parser class for MySQL parsing. */
abstract class MySQLParserBase extends Parser {

    // To parameterize the parsing process.
    int serverVersion = 0;
    HashSet<SqlMode> sqlModes = HashSet<SqlMode>();
    bool supportMle = true;

    MySQLParserBase(TokenStream input) : super(input)
    {
        this.serverVersion = 80200;
        this.sqlModes = SqlModes.sqlModeFromString("ANSI_QUOTES");
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
}
