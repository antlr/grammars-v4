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

    /**
     * Converts a mode string into individual mode flags.
     *
     * @param modes The input string to parse.
     */
    public void sqlModeFromString(string modes)
    {
        this.sqlModes = new HashSet<SqlMode>();

        var parts = modes.ToUpper().Split(",");
        foreach (var mode in parts)
        {
            if (mode == "ANSI" || mode == "DB2" || mode == "MAXDB" || mode == "MSSQL" || mode == "ORACLE" ||
            mode == "POSTGRESQL") {
                this.sqlModes.Add(SqlMode.AnsiQuotes);
                this.sqlModes.Add(SqlMode.PipesAsConcat);
                this.sqlModes.Add(SqlMode.IgnoreSpace);
            } else if (mode == "ANSI_QUOTES") {
                this.sqlModes.Add(SqlMode.AnsiQuotes);
            } else if (mode == "PIPES_AS_CONCAT") {
                this.sqlModes.Add(SqlMode.PipesAsConcat);
            } else if (mode == "NO_BACKSLASH_ESCAPES") {
                this.sqlModes.Add(SqlMode.NoBackslashEscapes);
            } else if (mode == "IGNORE_SPACE") {
                this.sqlModes.Add(SqlMode.IgnoreSpace);
            } else if (mode == "HIGH_NOT_PRECEDENCE" || mode == "MYSQL323" || mode == "MYSQL40") {
                this.sqlModes.Add(SqlMode.HighNotPrecedence);
            }
        }
    }
}
