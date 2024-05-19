/*
 * Copyright © 2024, Oracle and/or its affiliates
 */

import { Parser } from "antlr4ng";

import { SqlMode } from "./MySQLBaseLexer.js";

export abstract class MySQLBaseRecognizer extends Parser {

    // To parameterize the parsing process.
    public serverVersion = 0;
    public sqlModes = new Set<SqlMode>();

    /** Enable MRS specific language parts. */
    public supportMrs = true;

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

}
