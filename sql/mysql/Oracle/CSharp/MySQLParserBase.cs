/*
 * Copyright © 2024, Oracle and/or its affiliates
 */

using Antlr4.Runtime;
using System.Collections.Generic;
using System.IO;

public abstract class MySQLParserBase : Parser {

    // To parameterize the parsing process.
    public int serverVersion = 0;
    public HashSet<SqlMode> sqlModes = new HashSet<SqlMode>();

    /** Enable Multi Language Extension support. */
    public bool supportMle = true;

    protected MySQLParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput) : base(input, output, errorOutput)
    {
    }

    /**
     * Determines if the given SQL mode is currently active in the lexer.
     *
     * @param mode The mode to check.
     *
     * @returns True if the mode is one of the currently active modes.
     */
    public bool isSqlModeActive(SqlMode mode) {
        return this.sqlModes.Contains(mode);
    }

    public bool isPureIdentifier()
    {
        return this.isSqlModeActive(SqlMode.AnsiQuotes);
    }

    public bool isTextStringLiteral()
    {
        return !this.isSqlModeActive(SqlMode.AnsiQuotes);
    }

    public bool isStoredRoutineBody()
    {
        return serverVersion >= 80032 && supportMle;
    }

    public bool isSelectStatementWithInto()
    {
        return serverVersion >= 80024 && serverVersion < 80031;
    }
}
