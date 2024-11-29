/*
 * Copyright © 2024, Oracle and/or its affiliates
 */

import { Parser, TokenStream } from "antlr4ng";
import { SqlMode } from "./MySQLLexerBase.js";

export abstract class MySQLParserBase extends Parser {

    // To parameterize the parsing process.
    public serverVersion = 0;
    public sqlModes = new Set<SqlMode>();

    /** Enable Multi Language Extension support. */
    public supportMle = true;

    constructor(input: TokenStream) {
        super(input);
        this.serverVersion = 80200;
        this.sqlModeFromString("ANSI_QUOTES");
    }

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

    /**
     * Converts a mode string into individual mode flags.
     *
     * @param modes The input string to parse.
     */
    public sqlModeFromString(modes: string): void {
        this.sqlModes = new Set<SqlMode>();

        const parts = modes.toUpperCase().split(",");
        parts.forEach((mode: string) => {
            if (mode === "ANSI" || mode === "DB2" || mode === "MAXDB" || mode === "MSSQL" || mode === "ORACLE" ||
                mode === "POSTGRESQL") {
                this.sqlModes.add(SqlMode.AnsiQuotes).add(SqlMode.PipesAsConcat).add(SqlMode.IgnoreSpace);
            } else if (mode === "ANSI_QUOTES") {
                this.sqlModes.add(SqlMode.AnsiQuotes);
            } else if (mode === "PIPES_AS_CONCAT") {
                this.sqlModes.add(SqlMode.PipesAsConcat);
            } else if (mode === "NO_BACKSLASH_ESCAPES") {
                this.sqlModes.add(SqlMode.NoBackslashEscapes);
            } else if (mode === "IGNORE_SPACE") {
                this.sqlModes.add(SqlMode.IgnoreSpace);
            } else if (mode === "HIGH_NOT_PRECEDENCE" || mode === "MYSQL323" || mode === "MYSQL40") {
                this.sqlModes.add(SqlMode.HighNotPrecedence);
            }
        });
    }
}
