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
    /**
     * Determines if the given SQL mode is currently active in the lexer.
     *
     * @param mode The mode to check.
     *
     * @returns True if the mode is one of the currently active modes.
     */
    isSqlModeActive(mode) {
        return this.sqlModes.has(mode);
    }
    isPureIdentifier() {
        return this.isSqlModeActive(SqlMode.AnsiQuotes);
    }
    isTextStringLiteral() {
        return !this.isSqlModeActive(SqlMode.AnsiQuotes);
    }
    isStoredRoutineBody() {
        return this.serverVersion >= 80032 && this.supportMle;
    }
    isSelectStatementWithInto() {
        return this.serverVersion >= 80024 && this.serverVersion < 80031;
    }
}
