/*
 * Copyright © 2025, Oracle and/or its affiliates
 */

import { Parser, TokenStream } from "antlr4";
import MySQLParser from "./MySQLParser";
import SqlMode from "./SqlMode";
import SqlModes from "./SqlModes";

export default abstract class MySQLParserBase extends Parser {

    // To parameterize the parsing process.
    public serverVersion = 0;
    public sqlModes = new Set<SqlMode>();

    /** Enable Multi Language Extension support. */
    public supportMle = true;

    constructor(input: TokenStream) {
        super(input);
        this.serverVersion = 80200;
        this.sqlModes = SqlModes.sqlModeFromString("ANSI_QUOTES");
    }

    public isSqlModeActive(mode: SqlMode): boolean { return this.sqlModes.has(mode); }
    public isPureIdentifier(): boolean { return this.isSqlModeActive(SqlMode.AnsiQuotes); }
    public isTextStringLiteral(): boolean { return !this.isSqlModeActive(SqlMode.AnsiQuotes); }
    public isStoredRoutineBody(): boolean { return this.serverVersion >= 80032 && this.supportMle; }
    public isSelectStatementWithInto(): boolean { return this.serverVersion >= 80024 && this.serverVersion < 80031; }
    public isServerVersionGe80004(): boolean { return this.serverVersion >= 80004; }
    public isServerVersionGe80011(): boolean { return this.serverVersion >= 80011; }
    public isServerVersionGe80013(): boolean { return this.serverVersion >= 80013; }
    public isServerVersionGe80014(): boolean { return this.serverVersion >= 80014; }
    public isServerVersionGe80016(): boolean { return this.serverVersion >= 80016; }
    public isServerVersionGe80017(): boolean { return this.serverVersion >= 80017; }
    public isServerVersionGe80018(): boolean { return this.serverVersion >= 80018; }
    public isServerVersionGe80019(): boolean { return this.serverVersion >= 80019; }
    public isServerVersionGe80024(): boolean { return this.serverVersion >= 80024; }
    public isServerVersionGe80025(): boolean { return this.serverVersion >= 80025; }
    public isServerVersionGe80027(): boolean { return this.serverVersion >= 80027; }
    public isServerVersionGe80031(): boolean { return this.serverVersion >= 80031; }
    public isServerVersionGe80032(): boolean { return this.serverVersion >= 80032; }
    public isServerVersionGe80100(): boolean { return this.serverVersion >= 80100; }
    public isServerVersionGe80200(): boolean { return this.serverVersion >= 80200; }
    public isServerVersionLt80011(): boolean { return this.serverVersion < 80011; }
    public isServerVersionLt80012(): boolean { return this.serverVersion < 80012; }
    public isServerVersionLt80014(): boolean { return this.serverVersion < 80014; }
    public isServerVersionLt80016(): boolean { return this.serverVersion < 80016; }
    public isServerVersionLt80017(): boolean { return this.serverVersion < 80017; }
    public isServerVersionLt80024(): boolean { return this.serverVersion < 80024; }
    public isServerVersionLt80025(): boolean { return this.serverVersion < 80025; }
    public isServerVersionLt80031(): boolean { return this.serverVersion < 80031; }
}
