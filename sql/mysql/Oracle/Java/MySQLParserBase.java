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

    public boolean isSqlModeActive(SqlMode mode) { return this.sqlModes.contains(mode); }
    public boolean isPureIdentifier() { return this.isSqlModeActive(SqlMode.AnsiQuotes); }
    public boolean isTextStringLiteral() { return !this.isSqlModeActive(SqlMode.AnsiQuotes); }
    public boolean isStoredRoutineBody() { return serverVersion >= 80032 && supportMle; }
    public boolean isSelectStatementWithInto() { return serverVersion >= 80024 && serverVersion < 80031; }
    public boolean isServerVersionGe80004() { return this.serverVersion >= 80004; }
    public boolean isServerVersionGe80011() { return this.serverVersion >= 80011; }
    public boolean isServerVersionGe80013() { return this.serverVersion >= 80013; }
    public boolean isServerVersionGe80014() { return this.serverVersion >= 80014; }
    public boolean isServerVersionGe80016() { return this.serverVersion >= 80016; }
    public boolean isServerVersionGe80017() { return this.serverVersion >= 80017; }
    public boolean isServerVersionGe80018() { return this.serverVersion >= 80018; }
    public boolean isServerVersionGe80019() { return this.serverVersion >= 80019; }
    public boolean isServerVersionGe80024() { return this.serverVersion >= 80024; }
    public boolean isServerVersionGe80025() { return this.serverVersion >= 80025; }
    public boolean isServerVersionGe80027() { return this.serverVersion >= 80027; }
    public boolean isServerVersionGe80031() { return this.serverVersion >= 80031; }
    public boolean isServerVersionGe80032() { return this.serverVersion >= 80032; }
    public boolean isServerVersionGe80100() { return this.serverVersion >= 80100; }
    public boolean isServerVersionGe80200() { return this.serverVersion >= 80200; }
    public boolean isServerVersionLt80011() { return this.serverVersion < 80011; }
    public boolean isServerVersionLt80012() { return this.serverVersion < 80012; }
    public boolean isServerVersionLt80014() { return this.serverVersion < 80014; }
    public boolean isServerVersionLt80016() { return this.serverVersion < 80016; }
    public boolean isServerVersionLt80017() { return this.serverVersion < 80017; }
    public boolean isServerVersionLt80024() { return this.serverVersion < 80024; }
    public boolean isServerVersionLt80025() { return this.serverVersion < 80025; }
    public boolean isServerVersionLt80031() { return this.serverVersion < 80031; }

}
