/*
 * Copyright © 2024, Oracle and/or its affiliates
 */

/* eslint-disable no-underscore-dangle */
/* cspell: ignore antlr, longlong, ULONGLONG, MAXDB */

using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.IO;

/** The base lexer class provides a number of functions needed in actions in the lexer (grammar). */
public class MySQLLexerBase : Lexer {
    public int serverVersion = 0;
    public HashSet<SqlMode> sqlModes = new HashSet<SqlMode>();

    /** Enable Multi Language Extension support. */
    public bool supportMle = true;

    bool justEmittedDot = false;

    public HashSet<string> charSets = new HashSet<string>(); // Used to check repertoires.
    protected bool inVersionComment = false;

    private Queue<IToken> pendingTokens  = new Queue<IToken>();

    static string longString = "2147483647";
    static int longLength = 10;
    static string signedLongString = "-2147483648";
    static string longLongString = "9223372036854775807";
    static int longLongLength = 19;
    static string signedLongLongString = "-9223372036854775808";
    static int signedLongLongLength = 19;
    static string unsignedLongLongString = "18446744073709551615";
    static int unsignedLongLongLength = 20;

    public override string[] RuleNames => throw new NotImplementedException();

    public override IVocabulary Vocabulary => throw new NotImplementedException();

    public override string GrammarFileName => throw new NotImplementedException();


    protected MySQLLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        this.serverVersion = 80200;
        this.sqlModes = SqlModes.sqlModeFromString("ANSI_QUOTES");
    }

    public MySQLLexerBase(ICharStream input)
        : base(input)
    {
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
    public bool isSqlModeActive(SqlMode mode)
    {
        return this.sqlModes.Contains(mode);
    }

    /**
     * Resets the lexer by setting initial values to transient member, resetting the input stream position etc.
     */
    public override void Reset()
    {
        this.inVersionComment = false;
        base.Reset();
    }

    /**
     * Implements the multi token feature required in our lexer.
     * A lexer rule can emit more than a single token, if needed.
     *
     * @returns The next token in the token stream.
     */
    public override IToken NextToken()
    {
        // First respond with pending tokens to the next token request, if there are any.
        IToken pending;
        var not_empty = this.pendingTokens.TryDequeue(out pending);
        if (not_empty) {
            return pending;
        }

        // Let the main lexer class run the next token recognition.
        // This might create additional tokens again.
        var next = base.NextToken();

        not_empty = this.pendingTokens.TryDequeue(out pending);
        if (not_empty) {
            this.pendingTokens.Enqueue(next);
            return pending;
        }

        return next;
    }

    /**
     * Checks if the version number in the token text is less than or equal to the current server version.
     *
     * @param text The text from a matched token.
     * @returns True if so the number matches, otherwise false.
     */
    protected bool checkMySQLVersion(string text)
    {
        if (text.Length < 8) {// Minimum is: /*!12345
            return false;
        }

        // Skip version comment introducer.
        var version = Int32.Parse(text.Substring(3));
        if (version <= this.serverVersion) {
            this.inVersionComment = true;

            return true;
        }

        return false;
    }

    /**
     * Called when a keyword was consumed that represents an internal MySQL function and checks if that keyword is
     * followed by an open parenthesis. If not then it is not considered a keyword but treated like a normal identifier.
     *
     * @param proposed The token type to use if the check succeeds.
     *
     * @returns If a function call is found then return the proposed token type, otherwise just IDENTIFIER.
     */
    protected int determineFunction(int proposed)
    {
        // Skip any whitespace character if the sql mode says they should be ignored,
        // before actually trying to match the open parenthesis.
        var input = (char)this.InputStream.LA(1);
        if (this.isSqlModeActive(SqlMode.IgnoreSpace)) {
            while (input == ' ' || input == '\t' || input == '\r' || input == '\n') {
                this.Interpreter.Consume((ICharStream)this.InputStream);
                this.Channel = Hidden;
                this.Type = MySQLLexer.WHITESPACE;
                input = (char)this.InputStream.LA(1);
            }
        }

        return input == '(' ? proposed : MySQLLexer.IDENTIFIER;

    }

    /**
     * Checks the given text and determines the smallest number type from it. Code has been taken from sql_lex.cc.
     *
     * @param text The text to parse (which must be a number).
     *
     * @returns The token type for that text.
     */
    protected int determineNumericType(string text)
    {
        // The original code checks for leading +/- but actually that can never happen, neither in the
        // server parser (as a digit is used to trigger processing in the lexer) nor in our parser
        // as our rules are defined without signs. But we do it anyway for maximum compatibility.
        var length = text.Length - 1;
        if (length < MySQLLexerBase.longLength) { // quick normal case
            return MySQLLexer.INT_NUMBER;
        }

        var negative = false;
        var index = 0;
        if (text[index] == '+') { // Remove sign and pre-zeros
            ++index;
            --length;
        } else if (text[index] == '-') {
            ++index;
            --length;
            negative = true;
        }

        while (text[index] == '0' && length > 0) {
            ++index;
            --length;
        }

        if (length < MySQLLexerBase.longLength) {
            return MySQLLexer.INT_NUMBER;
        }

        int smaller;
        int bigger;
        string cmp;
        if (negative) {
            if (length == MySQLLexerBase.longLength) {
                cmp = MySQLLexerBase.signedLongString.Substring(1);
                smaller = MySQLLexer.INT_NUMBER; // If <= signed_long_str
                bigger = MySQLLexer.LONG_NUMBER; // If >= signed_long_str
            } else if (length < MySQLLexerBase.signedLongLongLength) {
                return MySQLLexer.LONG_NUMBER;
            } else if (length > MySQLLexerBase.signedLongLongLength) {
                return MySQLLexer.DECIMAL_NUMBER;
            } else {
                cmp = MySQLLexerBase.signedLongLongString.Substring(1);
                smaller = MySQLLexer.LONG_NUMBER; // If <= signed_longlong_str
                bigger = MySQLLexer.DECIMAL_NUMBER;
            }
        } else {
            if (length == MySQLLexerBase.longLength) {
                cmp = MySQLLexerBase.longString;
                smaller = MySQLLexer.INT_NUMBER;
                bigger = MySQLLexer.LONG_NUMBER;
            } else if (length < MySQLLexerBase.longLongLength) {
                return MySQLLexer.LONG_NUMBER;
            } else if (length > MySQLLexerBase.longLongLength) {
                if (length > MySQLLexerBase.unsignedLongLongLength) {
                    return MySQLLexer.DECIMAL_NUMBER;
                }
                cmp = MySQLLexerBase.unsignedLongLongString;
                smaller = MySQLLexer.ULONGLONG_NUMBER;
                bigger = MySQLLexer.DECIMAL_NUMBER;
            } else {
                cmp = MySQLLexerBase.longLongString;
                smaller = MySQLLexer.LONG_NUMBER;
                bigger = MySQLLexer.ULONGLONG_NUMBER;
            }
        }

        var otherIndex = 0;
        while (index < text.Length && cmp[otherIndex++] == text[index++]) {
            //
        }

        return text[index - 1] <= cmp[otherIndex - 1] ? smaller : bigger;
    }

    /**
     * Checks if the given text corresponds to a charset defined in the server (text is preceded by an underscore).
     *
     * @param text The text to check.
     *
     * @returns UNDERSCORE_CHARSET if so, otherwise IDENTIFIER.
     */
    protected int checkCharset(string text)
    {
        return this.charSets.Contains(text) ? MySQLLexer.UNDERSCORE_CHARSET : MySQLLexer.IDENTIFIER;
    }

    /**
     * Creates a DOT token in the token stream.
     */
    protected void emitDot()
    {
        var len = this.Text.Length;
        var t = this.TokenFactory.Create(new Tuple<ITokenSource, ICharStream>(this, (ICharStream)this.InputStream), MySQLLexer.DOT_SYMBOL,
            ".", this.Channel, this.TokenStartCharIndex, this.TokenStartCharIndex, this.Line, this.Column) as CommonToken;
        this.pendingTokens.Enqueue(t);
        t.Column = t.Column - len;
        ++this.Column;
        this.justEmittedDot = true;
    }

    public override IToken Emit()
    {
        var t = base.Emit();
        if (this.justEmittedDot) {
            var p = t as CommonToken;
            p.Text = p.Text.Substring(1);
            p.Column = p.Column + 1;
            p.StartIndex = p.StartIndex + 1;
            this.Column = this.Column - 1;
            this.justEmittedDot = false;
        }
        return t;
    }

    // Version-related methods
    public bool isServerVersionGe80011() => serverVersion >= 80011;
    public bool isServerVersionGe80013() => serverVersion >= 80013;
    public bool isServerVersionLt80014() => serverVersion < 80014;
    public bool isServerVersionGe80014() => serverVersion >= 80014;
    public bool isServerVersionGe80016() => serverVersion >= 80016;
    public bool isServerVersionGe80017() => serverVersion >= 80017;
    public bool isServerVersionGe80018() => serverVersion >= 80018;
    public bool isServerVersionLt80021() => serverVersion < 80021;
    public bool isServerVersionGe80021() => serverVersion >= 80021;
    public bool isServerVersionLt80022() => serverVersion < 80022;
    public bool isServerVersionGe80022() => serverVersion >= 80022;
    public bool isServerVersionLt80023() => serverVersion < 80023;
    public bool isServerVersionGe80023() => serverVersion >= 80023;
    public bool isServerVersionLt80024() => serverVersion < 80024;
    public bool isServerVersionGe80024() => serverVersion >= 80024;

    public bool isMasterCompressionAlgorithm() => serverVersion >= 80018 && isServerVersionLt80024();

    public bool isServerVersionLt80031() => serverVersion < 80031;

    // Functions for specific token types
    public void doLogicalOr()
    {
        Type = isSqlModeActive(SqlMode.PipesAsConcat) ? MySQLLexer.CONCAT_PIPES_SYMBOL : MySQLLexer.LOGICAL_OR_OPERATOR;
    }

    public void doIntNumber()
    {
        Type = determineNumericType(Text);
    }

    public void doAdddate() => Type = determineFunction(MySQLLexer.ADDDATE_SYMBOL);
    public void doBitAnd() => Type = determineFunction(MySQLLexer.BIT_AND_SYMBOL);
    public void doBitOr() => Type = determineFunction(MySQLLexer.BIT_OR_SYMBOL);
    public void doBitXor() => Type = determineFunction(MySQLLexer.BIT_XOR_SYMBOL);
    public void doCast() => Type = determineFunction(MySQLLexer.CAST_SYMBOL);
    public void doCount() => Type = determineFunction(MySQLLexer.COUNT_SYMBOL);
    public void doCurdate() => Type = determineFunction(MySQLLexer.CURDATE_SYMBOL);
    public void doCurrentDate() => Type = determineFunction(MySQLLexer.CURDATE_SYMBOL);
    public void doCurrentTime() => Type = determineFunction(MySQLLexer.CURTIME_SYMBOL);
    public void doCurtime() => Type = determineFunction(MySQLLexer.CURTIME_SYMBOL);
    public void doDateAdd() => Type = determineFunction(MySQLLexer.DATE_ADD_SYMBOL);
    public void doDateSub() => Type = determineFunction(MySQLLexer.DATE_SUB_SYMBOL);
    public void doExtract() => Type = determineFunction(MySQLLexer.EXTRACT_SYMBOL);
    public void doGroupConcat() => Type = determineFunction(MySQLLexer.GROUP_CONCAT_SYMBOL);
    public void doMax() => Type = determineFunction(MySQLLexer.MAX_SYMBOL);
    public void doMid() => Type = determineFunction(MySQLLexer.SUBSTRING_SYMBOL);
    public void doMin() => Type = determineFunction(MySQLLexer.MIN_SYMBOL);
    public void doNot() => Type = isSqlModeActive(SqlMode.HighNotPrecedence) ? MySQLLexer.NOT2_SYMBOL : MySQLLexer.NOT_SYMBOL;
    public void doNow() => Type = determineFunction(MySQLLexer.NOW_SYMBOL);
    public void doPosition() => Type = determineFunction(MySQLLexer.POSITION_SYMBOL);
    public void doSessionUser() => Type = determineFunction(MySQLLexer.USER_SYMBOL);
    public void doStddevSamp() => Type = determineFunction(MySQLLexer.STDDEV_SAMP_SYMBOL);
    public void doStddev() => Type = determineFunction(MySQLLexer.STD_SYMBOL);
    public void doStddevPop() => Type = determineFunction(MySQLLexer.STD_SYMBOL);
    public void doStd() => Type = determineFunction(MySQLLexer.STD_SYMBOL);
    public void doSubdate() => Type = determineFunction(MySQLLexer.SUBDATE_SYMBOL);
    public void doSubstr() => Type = determineFunction(MySQLLexer.SUBSTRING_SYMBOL);
    public void doSubstring() => Type = determineFunction(MySQLLexer.SUBSTRING_SYMBOL);
    public void doSum() => Type = determineFunction(MySQLLexer.SUM_SYMBOL);
    public void doSysdate() => Type = determineFunction(MySQLLexer.SYSDATE_SYMBOL);
    public void doSystemUser() => Type = determineFunction(MySQLLexer.USER_SYMBOL);
    public void doTrim() => Type = determineFunction(MySQLLexer.TRIM_SYMBOL);
    public void doVariance() => Type = determineFunction(MySQLLexer.VARIANCE_SYMBOL);
    public void doVarPop() => Type = determineFunction(MySQLLexer.VARIANCE_SYMBOL);
    public void doVarSamp() => Type = determineFunction(MySQLLexer.VAR_SAMP_SYMBOL);
    public void doUnderscoreCharset() => Type = checkCharset(Text);
    public bool doDollarQuotedStringText() => this.serverVersion >= 80034 && this.supportMle;
    
    public bool isVersionComment() => checkMySQLVersion(Text);

    public bool isBackTickQuotedId()
    {
        return !this.isSqlModeActive(SqlMode.NoBackslashEscapes);
    }

    public bool isDoubleQuotedText()
    {
        return !this.isSqlModeActive(SqlMode.NoBackslashEscapes);
    }

    public bool isSingleQuotedText()
    {
        return !this.isSqlModeActive(SqlMode.NoBackslashEscapes);
    }

    public void startInVersionComment()
    {
        inVersionComment = true;
    }

    public void endInVersionComment()
    {
        inVersionComment = false;
    }

    public bool isInVersionComment()
    {
        return inVersionComment;
    }
}
