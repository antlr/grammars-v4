/*
 * Copyright © 2024, Oracle and/or its affiliates
 */
import { Parser } from "antlr4";
import SqlMode from "./SqlMode.js";
import SqlModes from "./SqlModes.js";
export default class MySQLParserBase extends Parser {
    constructor(input) {
        super(input);
        // To parameterize the parsing process.
        this.serverVersion = 0;
        this.sqlModes = new Set();
        /** Enable Multi Language Extension support. */
        this.supportMle = true;
        this.serverVersion = 80200;
        this.sqlModes = SqlModes.sqlModeFromString("ANSI_QUOTES");
    }
    isSqlModeActive(mode) { return this.sqlModes.has(mode); }
    isPureIdentifier() { return this.isSqlModeActive(SqlMode.AnsiQuotes); }
    isTextStringLiteral() { return !this.isSqlModeActive(SqlMode.AnsiQuotes); }
    isStoredRoutineBody() { return this.serverVersion >= 80032 && this.supportMle; }
    isSelectStatementWithInto() { return this.serverVersion >= 80024 && this.serverVersion < 80031; }
    isServerVersionGe80004() { return this.serverVersion >= 80004; }
    isServerVersionGe80011() { return this.serverVersion >= 80011; }
    isServerVersionGe80013() { return this.serverVersion >= 80013; }
    isServerVersionGe80014() { return this.serverVersion >= 80014; }
    isServerVersionGe80016() { return this.serverVersion >= 80016; }
    isServerVersionGe80017() { return this.serverVersion >= 80017; }
    isServerVersionGe80018() { return this.serverVersion >= 80018; }
    isServerVersionGe80019() { return this.serverVersion >= 80019; }
    isServerVersionGe80024() { return this.serverVersion >= 80024; }
    isServerVersionGe80025() { return this.serverVersion >= 80025; }
    isServerVersionGe80027() { return this.serverVersion >= 80027; }
    isServerVersionGe80031() { return this.serverVersion >= 80031; }
    isServerVersionGe80032() { return this.serverVersion >= 80032; }
    isServerVersionGe80100() { return this.serverVersion >= 80100; }
    isServerVersionGe80200() { return this.serverVersion >= 80200; }
    isServerVersionLt80011() { return this.serverVersion < 80011; }
    isServerVersionLt80012() { return this.serverVersion < 80012; }
    isServerVersionLt80014() { return this.serverVersion < 80014; }
    isServerVersionLt80016() { return this.serverVersion < 80016; }
    isServerVersionLt80017() { return this.serverVersion < 80017; }
    isServerVersionLt80024() { return this.serverVersion < 80024; }
    isServerVersionLt80025() { return this.serverVersion < 80025; }
    isServerVersionLt80031() { return this.serverVersion < 80031; }
}
