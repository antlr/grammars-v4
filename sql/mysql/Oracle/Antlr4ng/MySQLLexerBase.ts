/*
 * Copyright © 2024, Oracle and/or its affiliates
 */

/* eslint-disable no-underscore-dangle */
/* cspell: ignore antlr, longlong, ULONGLONG, MAXDB */

import { CharStream, Lexer, Token } from "antlr4ng";
import { MySQLLexer } from "./MySQLLexer.js";
import SqlMode from "./SqlMode.js";
import SqlModes from "./SqlModes.js";

/** The base lexer class provides a number of functions needed in actions in the lexer (grammar). */
export abstract class MySQLLexerBase extends Lexer {
    public serverVersion = 0;
    public sqlModes = new Set<SqlMode>();

    /** Enable Multi Language Extension support. */
    public supportMle = true;

    public charSets: Set<string> = new Set(); // Used to check repertoires.
    protected inVersionComment = false;

    private pendingTokens: Token[] = [];
    private justEmittedDot: boolean;

    static #longString = "2147483647";
    static #longLength = 10;
    static #signedLongString = "-2147483648";
    static #longLongString = "9223372036854775807";
    static #longLongLength = 19;
    static #signedLongLongString = "-9223372036854775808";
    static #signedLongLongLength = 19;
    static #unsignedLongLongString = "18446744073709551615";
    static #unsignedLongLongLength = 20;

    constructor(input: CharStream) {
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
    public isSqlModeActive(mode: SqlMode): boolean {
        return this.sqlModes.has(mode);
    }

    /**
     * Resets the lexer by setting initial values to transient member, resetting the input stream position etc.
     */
    public reset(): void {
        this.inVersionComment = false;
        super.reset();
    }

    /**
     * Implements the multi token feature required in our lexer.
     * A lexer rule can emit more than a single token, if needed.
     *
     * @returns The next token in the token stream.
     */
    public nextToken(): Token {
        // First respond with pending tokens to the next token request, if there are any.
        let pending = this.pendingTokens.shift();
        if (pending) {
            return pending;
        }

        // Let the main lexer class run the next token recognition.
        // This might create additional tokens again.
        const next = super.nextToken();
        pending = this.pendingTokens.shift();
        if (pending) {
            this.pendingTokens.push(next);
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
    protected checkMySQLVersion(text: string): boolean {
        if (text.length < 8) {// Minimum is: /*!12345
            return false;
        }

        // Skip version comment introducer.
        const version = parseInt(text.substring(3), 10);
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
    protected determineFunction(proposed: number): number {
        // Skip any whitespace character if the sql mode says they should be ignored,
        // before actually trying to match the open parenthesis.
        let input = String.fromCharCode(this.inputStream.LA(1));
        if (this.isSqlModeActive(SqlMode.IgnoreSpace)) {
            while (input === " " || input === "\t" || input === "\r" || input === "\n") {
                this.interpreter.consume(this.inputStream);
                this.channel = Lexer.HIDDEN;
                this.type = MySQLLexer.WHITESPACE;
                input = String.fromCharCode(this.inputStream.LA(1));
            }
        }

        return input === "(" ? proposed : MySQLLexer.IDENTIFIER;

    }

    /**
     * Checks the given text and determines the smallest number type from it. Code has been taken from sql_lex.cc.
     *
     * @param text The text to parse (which must be a number).
     *
     * @returns The token type for that text.
     */
    protected determineNumericType(text: string): number {
        // The original code checks for leading +/- but actually that can never happen, neither in the
        // server parser (as a digit is used to trigger processing in the lexer) nor in our parser
        // as our rules are defined without signs. But we do it anyway for maximum compatibility.
        let length = text.length - 1;
        if (length < MySQLLexerBase.#longLength) { // quick normal case
            return MySQLLexer.INT_NUMBER;
        }

        let negative = false;
        let index = 0;
        if (text.charAt(index) === "+") { // Remove sign and pre-zeros
            ++index;
            --length;
        } else if (text.charAt(index) === "-") {
            ++index;
            --length;
            negative = true;
        }

        while (text.charAt(index) === "0" && length > 0) {
            ++index;
            --length;
        }

        if (length < MySQLLexerBase.#longLength) {
            return MySQLLexer.INT_NUMBER;
        }

        let smaller: number;
        let bigger: number;
        let cmp: string;
        if (negative) {
            if (length === MySQLLexerBase.#longLength) {
                cmp = MySQLLexerBase.#signedLongString.substring(1);
                smaller = MySQLLexer.INT_NUMBER; // If <= signed_long_str
                bigger = MySQLLexer.LONG_NUMBER; // If >= signed_long_str
            } else if (length < MySQLLexerBase.#signedLongLongLength) {
                return MySQLLexer.LONG_NUMBER;
            } else if (length > MySQLLexerBase.#signedLongLongLength) {
                return MySQLLexer.DECIMAL_NUMBER;
            } else {
                cmp = MySQLLexerBase.#signedLongLongString.substring(1);
                smaller = MySQLLexer.LONG_NUMBER; // If <= signed_longlong_str
                bigger = MySQLLexer.DECIMAL_NUMBER;
            }
        } else {
            if (length === MySQLLexerBase.#longLength) {
                cmp = MySQLLexerBase.#longString;
                smaller = MySQLLexer.INT_NUMBER;
                bigger = MySQLLexer.LONG_NUMBER;
            } else if (length < MySQLLexerBase.#longLongLength) {
                return MySQLLexer.LONG_NUMBER;
            } else if (length > MySQLLexerBase.#longLongLength) {
                if (length > MySQLLexerBase.#unsignedLongLongLength) {
                    return MySQLLexer.DECIMAL_NUMBER;
                }
                cmp = MySQLLexerBase.#unsignedLongLongString;
                smaller = MySQLLexer.ULONGLONG_NUMBER;
                bigger = MySQLLexer.DECIMAL_NUMBER;
            } else {
                cmp = MySQLLexerBase.#longLongString;
                smaller = MySQLLexer.LONG_NUMBER;
                bigger = MySQLLexer.ULONGLONG_NUMBER;
            }
        }

        let otherIndex = 0;
        while (index < text.length && cmp.charAt(otherIndex++) === text.charAt(index++)) {
            //
        }

        return text.charAt(index - 1) <= cmp.charAt(otherIndex - 1) ? smaller : bigger;
    }

    /**
     * Checks if the given text corresponds to a charset defined in the server (text is preceded by an underscore).
     *
     * @param text The text to check.
     *
     * @returns UNDERSCORE_CHARSET if so, otherwise IDENTIFIER.
     */
    protected checkCharset(text: string): number {
        return this.charSets.has(text) ? MySQLLexer.UNDERSCORE_CHARSET : MySQLLexer.IDENTIFIER;
    }

    /**
     * Creates a DOT token in the token stream.
     */
    protected emitDot(): void {
        this.pendingTokens.push(this.tokenFactory.create([this, this.inputStream], MySQLLexer.DOT_SYMBOL,
            ".", this.channel, this.tokenStartCharIndex, this.tokenStartCharIndex, this.line,
            this.column - this.text.length,
        ));
        ++this.tokenStartCharIndex;
        this.justEmittedDot = true;
    }

    public override emit(): Token
    {
        let t = super.emit();
        if (this.justEmittedDot) {
            t.column = t.column + 1;
            this.justEmittedDot = false;
        }
        return t;
    }

    public isServerVersionLt80024(): boolean
    {
        return this.serverVersion < 80024;
    }

    public isServerVersionGe80024(): boolean
    {
        return this.serverVersion >= 80024;
    }

    public isServerVersionGe80011(): boolean
    {
        return this.serverVersion >= 80011;
    }

    public isServerVersionGe80013(): boolean
    {
        return this.serverVersion >= 80013;
    }

    public isServerVersionLt80014(): boolean
    {
        return this.serverVersion < 80014;
    }

    public isServerVersionGe80014(): boolean
    {
        return this.serverVersion >= 80014;
    }

    public isServerVersionGe80017(): boolean
    {
        return this.serverVersion >= 80017;
    }


    public isServerVersionGe80018(): boolean { return this.serverVersion >= 80018; }

    public isMasterCompressionAlgorithm(): boolean { return this.serverVersion >= 80018 && this.isServerVersionLt80024(); }

    public isServerVersionLt80031(): boolean
    {
        return this.serverVersion < 80031;
    }

    public doLogicalOr(): void
    {
        this.type = this.isSqlModeActive(SqlMode.PipesAsConcat) ? MySQLLexer.CONCAT_PIPES_SYMBOL : MySQLLexer.LOGICAL_OR_OPERATOR;
    }

    public doIntNumber(): void
    {
        this.type = this.determineNumericType(this.text);
    }

    public doAdddate(): void
    {
        this.type = this.determineFunction(MySQLLexer.ADDDATE_SYMBOL);
    }

    public doBitAnd(): void
    {
        this.type = this.determineFunction(MySQLLexer.BIT_AND_SYMBOL);
    }

    public doBitOr(): void
    {
        this.type = this.determineFunction(MySQLLexer.BIT_OR_SYMBOL);
    }

    public doBitXor(): void
    {
        this.type = this.determineFunction(MySQLLexer.BIT_XOR_SYMBOL);
    }

    public doCast(): void
    {
        this.type = this.determineFunction(MySQLLexer.CAST_SYMBOL);
    }

    public doCount(): void
    {
        this.type = this.determineFunction(MySQLLexer.COUNT_SYMBOL);
    }

    public doCurdate(): void
    {
        this.type = this.determineFunction(MySQLLexer.CURDATE_SYMBOL);
    }

    public doCurrentDate(): void
    {
        this.type = this.determineFunction(MySQLLexer.CURDATE_SYMBOL);
    }

    public doCurrentTime(): void
    {
        this.type = this.determineFunction(MySQLLexer.CURTIME_SYMBOL);
    }

    public doCurtime(): void
    {
        this.type = this.determineFunction(MySQLLexer.CURTIME_SYMBOL);
    }

    public doDateAdd(): void
    {
        this.type = this.determineFunction(MySQLLexer.DATE_ADD_SYMBOL);
    }

    public doDateSub(): void
    {
        this.type = this.determineFunction(MySQLLexer.DATE_SUB_SYMBOL);
    }

    public doExtract(): void
    {
        this.type = this.determineFunction(MySQLLexer.EXTRACT_SYMBOL);
    }

    public doGroupConcat(): void
    {
        this.type = this.determineFunction(MySQLLexer.GROUP_CONCAT_SYMBOL);
    }

    public doMax(): void
    {
        this.type = this.determineFunction(MySQLLexer.MAX_SYMBOL);
    }

    public doMid(): void
    {
        this.type = this.determineFunction(MySQLLexer.SUBSTRING_SYMBOL);
    }

    public doMin(): void
    {
        this.type = this.determineFunction(MySQLLexer.MIN_SYMBOL);
    }

    public doNot(): void
    {
        this.type = this.isSqlModeActive(SqlMode.HighNotPrecedence) ? MySQLLexer.NOT2_SYMBOL: MySQLLexer.NOT_SYMBOL;
    }

    public doNow(): void
    {
        this.type = this.determineFunction(MySQLLexer.NOW_SYMBOL);
    }

    public doPosition(): void
    {
        this.type = this.determineFunction(MySQLLexer.POSITION_SYMBOL);
    }

    public doSessionUser(): void
    {
        this.type = this.determineFunction(MySQLLexer.USER_SYMBOL);
    }

    public doStddevSamp(): void
    {
        this.type = this.determineFunction(MySQLLexer.STDDEV_SAMP_SYMBOL);
    }

    public doStddev(): void
    {
        this.type = this.determineFunction(MySQLLexer.STD_SYMBOL);
    }

    public doStddevPop(): void
    {
        this.type = this.determineFunction(MySQLLexer.STD_SYMBOL);
    }

    public doStd(): void
    {
        this.type = this.determineFunction(MySQLLexer.STD_SYMBOL);
    }

    public doSubdate(): void
    {
        this.type = this.determineFunction(MySQLLexer.SUBDATE_SYMBOL);
    }

    public doSubstr(): void
    {
        this.type = this.determineFunction(MySQLLexer.SUBSTRING_SYMBOL);
    }

    public doSubstring(): void
    {
        this.type = this.determineFunction(MySQLLexer.SUBSTRING_SYMBOL);
    }

    public doSum(): void
    {
        this.type = this.determineFunction(MySQLLexer.SUM_SYMBOL);
    }

    public doSysdate(): void
    {
        this.type = this.determineFunction(MySQLLexer.SYSDATE_SYMBOL);
    }

    public doSystemUser(): void
    {
        this.type = this.determineFunction(MySQLLexer.USER_SYMBOL);
    }

    public doTrim(): void
    {
        this.type = this.determineFunction(MySQLLexer.TRIM_SYMBOL);
    }

    public doVariance(): void
    {
        this.type = this.determineFunction(MySQLLexer.VARIANCE_SYMBOL);
    }

    public doVarPop(): void
    {
        this.type = this.determineFunction(MySQLLexer.VARIANCE_SYMBOL);
    }

    public doVarSamp(): void
    {
        this.type = this.determineFunction(MySQLLexer.VAR_SAMP_SYMBOL);
    }

    public doUnderscoreCharset(): void
    {
        this.type = this.checkCharset(this.text);
    }

    public isVersionComment(): boolean
    {
        return this.checkMySQLVersion(this.text);
    }

    public isBackTickQuotedId(): boolean
    {
        return !this.isSqlModeActive(SqlMode.NoBackslashEscapes);
    }

    public isDoubleQuotedText(): boolean
    {
        return !this.isSqlModeActive(SqlMode.NoBackslashEscapes);
    }

    public isSingleQuotedText(): boolean
    {
        return !this.isSqlModeActive(SqlMode.NoBackslashEscapes);
    }

    public startInVersionComment(): void
    {
        this.inVersionComment = true;
    }

    public endInVersionComment(): void
    {
        this.inVersionComment = false;
    }

    public isInVersionComment(): boolean
    {
        return this.inVersionComment;
    }
}
