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

    bool isSqlModeActive(SqlMode mode) { return sqlModes.contains(mode); }
    bool isPureIdentifier() { return isSqlModeActive(SqlMode.ansiQuotes); }
    bool isTextStringLiteral() { return !isSqlModeActive(SqlMode.ansiQuotes); }
    bool isStoredRoutineBody() { return serverVersion >= 80032 && supportMle; }
    bool isSelectStatementWithInto() { return serverVersion >= 80024 && serverVersion < 80031; }
    bool isServerVersionGe80004() { return this.serverVersion >= 80004; }
    bool isServerVersionGe80011() { return this.serverVersion >= 80011; }
    bool isServerVersionGe80013() { return this.serverVersion >= 80013; }
    bool isServerVersionGe80014() { return this.serverVersion >= 80014; }
    bool isServerVersionGe80016() { return this.serverVersion >= 80016; }
    bool isServerVersionGe80017() { return this.serverVersion >= 80017; }
    bool isServerVersionGe80018() { return this.serverVersion >= 80018; }
    bool isServerVersionGe80019() { return this.serverVersion >= 80019; }
    bool isServerVersionGe80024() { return this.serverVersion >= 80024; }
    bool isServerVersionGe80025() { return this.serverVersion >= 80025; }
    bool isServerVersionGe80027() { return this.serverVersion >= 80027; }
    bool isServerVersionGe80031() { return this.serverVersion >= 80031; }
    bool isServerVersionGe80032() { return this.serverVersion >= 80032; }
    bool isServerVersionGe80100() { return this.serverVersion >= 80100; }
    bool isServerVersionGe80200() { return this.serverVersion >= 80200; }
    bool isServerVersionLt80011() { return this.serverVersion < 80011; }
    bool isServerVersionLt80012() { return this.serverVersion < 80012; }
    bool isServerVersionLt80014() { return this.serverVersion < 80014; }
    bool isServerVersionLt80016() { return this.serverVersion < 80016; }
    bool isServerVersionLt80017() { return this.serverVersion < 80017; }
    bool isServerVersionLt80024() { return this.serverVersion < 80024; }
    bool isServerVersionLt80025() { return this.serverVersion < 80025; }
    bool isServerVersionLt80031() { return this.serverVersion < 80031; }
}
