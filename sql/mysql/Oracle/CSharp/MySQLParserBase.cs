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
        this.sqlModes = SqlModes.sqlModeFromString("ANSI_QUOTES");
    }

    public bool isSqlModeActive(SqlMode mode) { return this.sqlModes.Contains(mode); }
    public bool isPureIdentifier() { return this.isSqlModeActive(SqlMode.AnsiQuotes); }
    public bool isTextStringLiteral() { return !this.isSqlModeActive(SqlMode.AnsiQuotes); }
    public bool isStoredRoutineBody() { return serverVersion >= 80032 && supportMle; }
    public bool isSelectStatementWithInto() { return serverVersion >= 80024 && serverVersion < 80031; }
    public bool isServerVersionGe80004() { return this.serverVersion >= 80004; }
    public bool isServerVersionGe80011() { return this.serverVersion >= 80011; }
    public bool isServerVersionGe80013() { return this.serverVersion >= 80013; }
    public bool isServerVersionGe80014() { return this.serverVersion >= 80014; }
    public bool isServerVersionGe80016() { return this.serverVersion >= 80016; }
    public bool isServerVersionGe80017() { return this.serverVersion >= 80017; }
    public bool isServerVersionGe80018() { return this.serverVersion >= 80018; }
    public bool isServerVersionGe80019() { return this.serverVersion >= 80019; }
    public bool isServerVersionGe80024() { return this.serverVersion >= 80024; }
    public bool isServerVersionGe80025() { return this.serverVersion >= 80025; }
    public bool isServerVersionGe80027() { return this.serverVersion >= 80027; }
    public bool isServerVersionGe80031() { return this.serverVersion >= 80031; }
    public bool isServerVersionGe80032() { return this.serverVersion >= 80032; }
    public bool isServerVersionGe80100() { return this.serverVersion >= 80100; }
    public bool isServerVersionGe80200() { return this.serverVersion >= 80200; }
    public bool isServerVersionLt80011() { return this.serverVersion < 80011; }
    public bool isServerVersionLt80012() { return this.serverVersion < 80012; }
    public bool isServerVersionLt80014() { return this.serverVersion < 80014; }
    public bool isServerVersionLt80016() { return this.serverVersion < 80016; }
    public bool isServerVersionLt80017() { return this.serverVersion < 80017; }
    public bool isServerVersionLt80024() { return this.serverVersion < 80024; }
    public bool isServerVersionLt80025() { return this.serverVersion < 80025; }
    public bool isServerVersionLt80031() { return this.serverVersion < 80031; }
}
