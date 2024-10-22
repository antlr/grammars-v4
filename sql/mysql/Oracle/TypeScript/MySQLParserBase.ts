/*
 * Copyright © 2024, Oracle and/or its affiliates
 */

import { Parser } from "antlr4";
import MySQLParser from "./MySQLParser";
import SqlMode from "./SqlMode";

export default abstract class MySQLParserBase extends Parser {

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
}
