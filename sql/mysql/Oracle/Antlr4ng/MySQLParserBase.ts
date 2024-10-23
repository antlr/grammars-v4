/*
 * Copyright © 2024, Oracle and/or its affiliates
 */

import { Parser } from "antlr4ng";

import { SqlMode } from "./MySQLLexerBase.js";

export abstract class MySQLParserBase extends Parser {

    // To parameterize the parsing process.
    public serverVersion = 0;
    public sqlModes = new Set<SqlMode>();

    /** Enable Multi Language Extension support. */
    public supportMle = true;

    /**
     * Determines if the given SQL mode is currently active in the lexer.
     *
     * @param mode The mode to check.
     *
     * @returns True if the mode is one of the currently active modes.
     */
    public isSqlModeActive(mode: SqlMode): boolean {
        return this.sqlModes.has(mode);
    }

    public isPureIdentifier(): boolean
    {
	return this.isSqlModeActive(SqlMode.AnsiQuotes);
    }

    public isTextStringLiteral(): boolean
    {
	return !this.isSqlModeActive(SqlMode.AnsiQuotes);
    }

    public isStoredRoutineBody(): boolean
    {
	return this.serverVersion >= 80032 && this.supportMle;
    }

    public isSelectStatementWithInto(): boolean
    {
	return this.serverVersion >= 80024 && this.serverVersion < 80031;
    }
}
