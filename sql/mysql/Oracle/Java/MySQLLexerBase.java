/*
 * Copyright 2024, Oracle and/or its affiliates
 */

/* eslint-disable no-underscore-dangle */
/* cspell: ignore antlr, longlong, ULONGLONG, MAXDB */

import org.antlr.v4.runtime.*;
import java.util.*;


/** The base lexer class provides a number of functions needed in actions in the lexer (grammar). */
public abstract class MySQLLexerBase extends Lexer {

    public MySQLLexerBase(CharStream input) {
	super(input);
    }

    public int serverVersion = 0;
    public Set<SqlMode> sqlModes = new HashSet<>();

    /** Enable Multi Language Extension support. */
    public boolean supportMle = true;

    public Set<String> charSets = new HashSet<>(); // Used to check repertoires.
    protected boolean inVersionComment = false;

    private Deque<Token> pendingTokens = new ArrayDeque<>();

    static String longString = "2147483647";
    static int longLength = 10;
    static String signedLongString = "-2147483648";
    static String longLongString = "9223372036854775807";
    static int longLongLength = 19;
    static String signedLongLongString = "-9223372036854775808";
    static int signedLongLongLength = 19;
    static String unsignedLongLongString = "18446744073709551615";
    static int unsignedLongLongLength = 20;

    private boolean justEmitedDot = false;

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

    /**
     * Converts a mode string into individual mode flags.
     *
     * @param modes The input string to parse.
     */
    public void sqlModeFromString(String modes) {
        this.sqlModes = new HashSet<>();

        String[] parts = modes.toUpperCase().split(",");
        for (String mode : parts) {
            switch (mode) {
                case "ANSI":
                case "DB2":
                case "MAXDB":
                case "MSSQL":
                case "ORACLE":
                case "POSTGRESQL":
                    this.sqlModes.add(SqlMode.AnsiQuotes);
                    this.sqlModes.add(SqlMode.PipesAsConcat);
                    this.sqlModes.add(SqlMode.IgnoreSpace);
                    break;
                case "ANSI_QUOTES":
                    this.sqlModes.add(SqlMode.AnsiQuotes);
                    break;
                case "PIPES_AS_CONCAT":
                    this.sqlModes.add(SqlMode.PipesAsConcat);
                    break;
                case "NO_BACKSLASH_ESCAPES":
                    this.sqlModes.add(SqlMode.NoBackslashEscapes);
                    break;
                case "IGNORE_SPACE":
                    this.sqlModes.add(SqlMode.IgnoreSpace);
                    break;
                case "HIGH_NOT_PRECEDENCE":
                case "MYSQL323":
                case "MYSQL40":
                    this.sqlModes.add(SqlMode.HighNotPrecedence);
                    break;
            }
        }
    }

    @Override
    public void reset() {
        this.inVersionComment = false;
        super.reset();
    }

    @Override
    public Token nextToken() {
        Token pending = pendingTokens.pollLast();
        if (pending != null) {
            return pending;
        }

        Token next = super.nextToken();

        pending = pendingTokens.pollLast();
        if (pending != null) {
            pendingTokens.push(next);
            return pending;
        }

        return next;
    }

    protected boolean checkMySQLVersion(String text) {
        if (text.length() < 8) { // Minimum is: /*!12345
            return false;
        }

        int version = Integer.parseInt(text.substring(3));
        if (version <= this.serverVersion) {
            this.inVersionComment = true;
            return true;
        }

        return false;
    }

    protected int determineFunction(int proposed) {
        char input = (char) this._input.LA(1);
        if (this.isSqlModeActive(SqlMode.IgnoreSpace)) {
            while (input == ' ' || input == '\t' || input == '\r' || input == '\n') {
                this.getInterpreter().consume(this._input);
                this._channel = HIDDEN;
                this._type = MySQLLexer.WHITESPACE;
                input = (char) this._input.LA(1);
            }
        }
        return input == '(' ? proposed : MySQLLexer.IDENTIFIER;
    }

    protected int determineNumericType(String text) {
        int length = text.length() - 1;
        if (length < MySQLLexerBase.longLength) {
            return MySQLLexer.INT_NUMBER;
        }

        boolean negative = false;
        int index = 0;
        if (text.charAt(index) == '+') {
            ++index;
            --length;
        } else if (text.charAt(index) == '-') {
            ++index;
            --length;
            negative = true;
        }

        while (text.charAt(index) == '0' && length > 0) {
            ++index;
            --length;
        }

        if (length < MySQLLexerBase.longLength) {
            return MySQLLexer.INT_NUMBER;
        }

        String cmp;
        int smaller;
        int bigger;
        if (negative) {
            if (length == MySQLLexerBase.longLength) {
                cmp = MySQLLexerBase.signedLongString.substring(1);
                smaller = MySQLLexer.INT_NUMBER;
                bigger = MySQLLexer.LONG_NUMBER;
            } else if (length < MySQLLexerBase.signedLongLongLength) {
                return MySQLLexer.LONG_NUMBER;
            } else if (length > MySQLLexerBase.signedLongLongLength) {
                return MySQLLexer.DECIMAL_NUMBER;
            } else {
                cmp = MySQLLexerBase.signedLongLongString.substring(1);
                smaller = MySQLLexer.LONG_NUMBER;
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

        int otherIndex = 0;
        while (index < text.length() && cmp.charAt(otherIndex++) == text.charAt(index++)) {}

        return text.charAt(index - 1) <= cmp.charAt(otherIndex - 1) ? smaller : bigger;
    }

    protected int checkCharset(String text) {
        return this.charSets.contains(text) ? MySQLLexer.UNDERSCORE_CHARSET : MySQLLexer.IDENTIFIER;
    }

    protected void emitDot() {
        pendingTokens.push(this._factory.create(this._tokenFactorySourcePair, MySQLLexer.DOT_SYMBOL,
                this.getText(), this._channel, this._tokenStartCharIndex, this._tokenStartCharIndex, this.getLine(), this.getCharPositionInLine()));
        ++this._tokenStartCharPositionInLine;
        this.justEmitedDot = true;
    }

    public boolean isServerVersionLt80024() { return serverVersion < 80024; }
    public boolean isServerVersionGe80024() { return serverVersion >= 80024; }
    public boolean isServerVersionGe80011() { return serverVersion >= 80011; }
    public boolean isServerVersionGe80013() { return serverVersion >= 80013; }
    public boolean isServerVersionLt80014() { return serverVersion < 80014; }
    public boolean isServerVersionGe80014() { return serverVersion >= 80014; }
    public boolean isServerVersionGe80017() { return serverVersion >= 80017; }
    public boolean isServerVersionGe80018() { return serverVersion >= 80018; }
    public boolean isMasterCompressionAlgorithm() { return serverVersion >= 80018 && isServerVersionLt80024(); }
    public boolean isServerVersionLt80031() { return serverVersion < 80031; }

    public void doLogicalOr() {
        this._type = isSqlModeActive(SqlMode.PipesAsConcat) ? MySQLLexer.CONCAT_PIPES_SYMBOL : MySQLLexer.LOGICAL_OR_OPERATOR;
    }

    public void doIntNumber() {
        this._type = determineNumericType(this.getText());
    }

    public void doAdddate() { this._type = determineFunction(MySQLLexer.ADDDATE_SYMBOL); }
    public void doBitAnd() { this._type = determineFunction(MySQLLexer.BIT_AND_SYMBOL); }
    public void doBitOr() { this._type = determineFunction(MySQLLexer.BIT_OR_SYMBOL); }
    public void doBitXor() { this._type = determineFunction(MySQLLexer.BIT_XOR_SYMBOL); }
    public void doCast() { this._type = determineFunction(MySQLLexer.CAST_SYMBOL); }
    public void doCount() { this._type = determineFunction(MySQLLexer.COUNT_SYMBOL); }
    public void doCurdate() { this._type = determineFunction(MySQLLexer.CURDATE_SYMBOL); }
    public void doCurrentDate() { this._type = determineFunction(MySQLLexer.CURDATE_SYMBOL); }
    public void doCurrentTime() { this._type = determineFunction(MySQLLexer.CURTIME_SYMBOL); }
    public void doCurtime() { this._type = determineFunction(MySQLLexer.CURTIME_SYMBOL); }
    public void doDateAdd() { this._type = determineFunction(MySQLLexer.DATE_ADD_SYMBOL); }
    public void doDateSub() { this._type = determineFunction(MySQLLexer.DATE_SUB_SYMBOL); }
    public void doExtract() { this._type = determineFunction(MySQLLexer.EXTRACT_SYMBOL); }
    public void doGroupConcat() { this._type = determineFunction(MySQLLexer.GROUP_CONCAT_SYMBOL); }
    public void doMax() { this._type = determineFunction(MySQLLexer.MAX_SYMBOL); }
    public void doMid() { this._type = determineFunction(MySQLLexer.SUBSTRING_SYMBOL); }
    public void doMin() { this._type = determineFunction(MySQLLexer.MIN_SYMBOL); }
    public void doNot() { this._type = isSqlModeActive(SqlMode.HighNotPrecedence) ? MySQLLexer.NOT2_SYMBOL : MySQLLexer.NOT_SYMBOL; }
    public void doNow() { this._type = determineFunction(MySQLLexer.NOW_SYMBOL); }
    public void doPosition() { this._type = determineFunction(MySQLLexer.POSITION_SYMBOL); }
    public void doSessionUser() { this._type = determineFunction(MySQLLexer.USER_SYMBOL); }
    public void doStddevSamp() { this._type = determineFunction(MySQLLexer.STDDEV_SAMP_SYMBOL); }
    public void doStddev() { this._type = determineFunction(MySQLLexer.STD_SYMBOL); }
    public void doStddevPop() { this._type = determineFunction(MySQLLexer.STD_SYMBOL); }
    public void doStd() { this._type = determineFunction(MySQLLexer.STD_SYMBOL); }
    public void doSubdate() { this._type = determineFunction(MySQLLexer.SUBDATE_SYMBOL); }
    public void doSubstr() { this._type = determineFunction(MySQLLexer.SUBSTRING_SYMBOL); }
    public void doSubstring() { this._type = determineFunction(MySQLLexer.SUBSTRING_SYMBOL); }
    public void doSum() { this._type = determineFunction(MySQLLexer.SUM_SYMBOL); }
    public void doSysdate() { this._type = determineFunction(MySQLLexer.SYSDATE_SYMBOL); }
    public void doSystemUser() { this._type = determineFunction(MySQLLexer.USER_SYMBOL); }
    public void doTrim() { this._type = determineFunction(MySQLLexer.TRIM_SYMBOL); }
    public void doVariance() { this._type = determineFunction(MySQLLexer.VARIANCE_SYMBOL); }
    public void doVarPop() { this._type = determineFunction(MySQLLexer.VARIANCE_SYMBOL); }
    public void doVarSamp() { this._type = determineFunction(MySQLLexer.VAR_SAMP_SYMBOL); }
    public void doUnderscoreCharset() { this._type = checkCharset(this.getText()); }

    public boolean isVersionComment() {
        return checkMySQLVersion(this.getText());
    }

    public boolean isBackTickQuotedId()
    {
	return !this.isSqlModeActive(SqlMode.NoBackslashEscapes);
    }

    public boolean isDoubleQuotedText()
    {
	return !this.isSqlModeActive(SqlMode.NoBackslashEscapes);
    }

    public boolean isSingleQuotedText()
    {
	return !this.isSqlModeActive(SqlMode.NoBackslashEscapes);
    }

    @Override
    public Token emit() {
        Token t = this._factory.create(this._tokenFactorySourcePair,
                this._type, this.getText() != null ? (this.justEmitedDot ? this.getText().substring(1) : this.getText()) : null, this._channel, this._tokenStartCharIndex + (this.justEmitedDot ? 1 : 0), this.getCharIndex() - 1, this.getLine(), this.getCharPositionInLine());
        this.justEmitedDot = false;
        super.emit(t);
        return t;
    }

    public void startInVersionComment()
    {
	inVersionComment = true;
    }

    public void endInVersionComment()
    {
	inVersionComment = false;
    }

    public boolean isInVersionComment()
    {
	return inVersionComment;
    }
}
