/*
 * Copyright 2024, Oracle and/or its affiliates
 */

import org.antlr.v4.runtime.*;
import java.util.*;
import java.io.*;

public abstract class MySQLParserBase extends Parser {

    // To parameterize the parsing process.
    public int serverVersion = 0;
    public Set<SqlMode> sqlModes = new HashSet<>();

    /** Enable Multi Language Extension support. */
    public boolean supportMle = true;

    protected MySQLParserBase(TokenStream input) {
	super(input);
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
    public boolean isSqlModeActive(SqlMode mode) {
	return this.sqlModes.contains(mode);
    }

    public boolean isPureIdentifier()
    {
	return this.isSqlModeActive(SqlMode.AnsiQuotes);
    }

    public boolean isTextStringLiteral()
    {
	return !this.isSqlModeActive(SqlMode.AnsiQuotes);
    }

    public boolean isStoredRoutineBody()
    {
	return serverVersion >= 80032 && supportMle;
    }

    public boolean isSelectStatementWithInto()
    {
	return serverVersion >= 80024 && serverVersion < 80031;
    }
}
