/*
 * Copyright © 2024, Oracle and/or its affiliates
 */
var __classPrivateFieldGet = (this && this.__classPrivateFieldGet) || function (receiver, state, kind, f) {
    if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a getter");
    if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot read private member from an object whose class did not declare it");
    return kind === "m" ? f : kind === "a" ? f.call(receiver) : f ? f.value : state.get(receiver);
};
var _a, _MySQLLexerBase_longString, _MySQLLexerBase_longLength, _MySQLLexerBase_signedLongString, _MySQLLexerBase_longLongString, _MySQLLexerBase_longLongLength, _MySQLLexerBase_signedLongLongString, _MySQLLexerBase_signedLongLongLength, _MySQLLexerBase_unsignedLongLongString, _MySQLLexerBase_unsignedLongLongLength;
/* eslint-disable no-underscore-dangle */
/* cspell: ignore antlr, longlong, ULONGLONG, MAXDB */
import { Lexer, Token } from "antlr4";
import { CommonToken } from "antlr4";
import MySQLLexer from "./MySQLLexer.js";
import SqlMode from "./SqlMode.js";
import SqlModes from "./SqlModes.js";
/** The base lexer class provides a number of functions needed in actions in the lexer (grammar). */
class MySQLLexerBase extends Lexer {
    constructor(input) {
        super(input);
        this.serverVersion = 0;
        this.sqlModes = new Set();
        /** Enable Multi Language Extension support. */
        this.supportMle = true;
        this.justEmittedDot = false;
        this.charSets = new Set(); // Used to check repertoires.
        this.inVersionComment = false;
        this.pendingTokens = [];
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
    isSqlModeActive(mode) {
        return this.sqlModes.has(mode);
    }
    /**
     * Resets the lexer by setting initial values to transient member, resetting the input stream position etc.
     */
    reset() {
        this.inVersionComment = false;
        super.reset();
    }
    /**
     * Implements the multi token feature required in our lexer.
     * A lexer rule can emit more than a single token, if needed.
     *
     * @returns The next token in the token stream.
     */
    nextToken() {
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
    checkMySQLVersion(text) {
        if (text.length < 8) { // Minimum is: /*!12345
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
    determineFunction(proposed) {
        // Skip any whitespace character if the sql mode says they should be ignored,
        // before actually trying to match the open parenthesis.
        let input = String.fromCharCode(this._input.LA(1));
        if (this.isSqlModeActive(SqlMode.IgnoreSpace)) {
            while (input === " " || input === "\t" || input === "\r" || input === "\n") {
                this._interp.consume(this._input);
                //                this.channel = Lexer.HIDDEN;
                this._type = MySQLLexer.WHITESPACE;
                input = String.fromCharCode(this._input.LA(1));
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
    determineNumericType(text) {
        // The original code checks for leading +/- but actually that can never happen, neither in the
        // server parser (as a digit is used to trigger processing in the lexer) nor in our parser
        // as our rules are defined without signs. But we do it anyway for maximum compatibility.
        let length = text.length - 1;
        if (length < __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_longLength)) { // quick normal case
            return MySQLLexer.INT_NUMBER;
        }
        let negative = false;
        let index = 0;
        if (text.charAt(index) === "+") { // Remove sign and pre-zeros
            ++index;
            --length;
        }
        else if (text.charAt(index) === "-") {
            ++index;
            --length;
            negative = true;
        }
        while (text.charAt(index) === "0" && length > 0) {
            ++index;
            --length;
        }
        if (length < __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_longLength)) {
            return MySQLLexer.INT_NUMBER;
        }
        let smaller;
        let bigger;
        let cmp;
        if (negative) {
            if (length === __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_longLength)) {
                cmp = __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_signedLongString).substring(1);
                smaller = MySQLLexer.INT_NUMBER; // If <= signed_long_str
                bigger = MySQLLexer.LONG_NUMBER; // If >= signed_long_str
            }
            else if (length < __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_signedLongLongLength)) {
                return MySQLLexer.LONG_NUMBER;
            }
            else if (length > __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_signedLongLongLength)) {
                return MySQLLexer.DECIMAL_NUMBER;
            }
            else {
                cmp = __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_signedLongLongString).substring(1);
                smaller = MySQLLexer.LONG_NUMBER; // If <= signed_longlong_str
                bigger = MySQLLexer.DECIMAL_NUMBER;
            }
        }
        else {
            if (length === __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_longLength)) {
                cmp = __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_longString);
                smaller = MySQLLexer.INT_NUMBER;
                bigger = MySQLLexer.LONG_NUMBER;
            }
            else if (length < __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_longLongLength)) {
                return MySQLLexer.LONG_NUMBER;
            }
            else if (length > __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_longLongLength)) {
                if (length > __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_unsignedLongLongLength)) {
                    return MySQLLexer.DECIMAL_NUMBER;
                }
                cmp = __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_unsignedLongLongString);
                smaller = MySQLLexer.ULONGLONG_NUMBER;
                bigger = MySQLLexer.DECIMAL_NUMBER;
            }
            else {
                cmp = __classPrivateFieldGet(_a, _a, "f", _MySQLLexerBase_longLongString);
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
    checkCharset(text) {
        return this.charSets.has(text) ? MySQLLexer.UNDERSCORE_CHARSET : MySQLLexer.IDENTIFIER;
    }
    /**
     * Creates a DOT token in the token stream.
     */
    emitDot() {
        let len = this.text.length;
        let t = new CommonToken([this, this._input], MySQLLexer.DOT_SYMBOL, 0, this._tokenStartCharIndex, this._tokenStartCharIndex);
        this.pendingTokens.push(t);
        t.text = ".";
        t.column = t.column - len;
        ++this._tokenStartCharIndex;
	this.justEmittedDot = true;
    }
    emit() {
        let t = super.emit();
	if (this.justEmittedDot) {
            t.column = t.column + 1;
            this.justEmittedDot = false;
        }
        return t;
    }
    isServerVersionLt80024() {
        return this.serverVersion < 80024;
    }
    isServerVersionGe80024() {
        return this.serverVersion >= 80024;
    }
    isServerVersionGe80011() {
        return this.serverVersion >= 80011;
    }
    isServerVersionGe80013() {
        return this.serverVersion >= 80013;
    }
    isServerVersionLt80014() {
        return this.serverVersion < 80014;
    }
    isServerVersionGe80014() {
        return this.serverVersion >= 80014;
    }
    isServerVersionGe80017() {
        return this.serverVersion >= 80017;
    }
    isServerVersionGe80018() { return this.serverVersion >= 80018; }
    isMasterCompressionAlgorithm() { return this.serverVersion >= 80018 && this.isServerVersionLt80024(); }
    isServerVersionLt80031() {
        return this.serverVersion < 80031;
    }
    doLogicalOr() {
        this._type = this.isSqlModeActive(SqlMode.PipesAsConcat) ? MySQLLexer.CONCAT_PIPES_SYMBOL : MySQLLexer.LOGICAL_OR_OPERATOR;
    }
    doIntNumber() {
        this._type = this.determineNumericType(this.text);
    }
    doAdddate() {
        this._type = this.determineFunction(MySQLLexer.ADDDATE_SYMBOL);
    }
    doBitAnd() {
        this._type = this.determineFunction(MySQLLexer.BIT_AND_SYMBOL);
    }
    doBitOr() {
        this._type = this.determineFunction(MySQLLexer.BIT_OR_SYMBOL);
    }
    doBitXor() {
        this._type = this.determineFunction(MySQLLexer.BIT_XOR_SYMBOL);
    }
    doCast() {
        this._type = this.determineFunction(MySQLLexer.CAST_SYMBOL);
    }
    doCount() {
        this._type = this.determineFunction(MySQLLexer.COUNT_SYMBOL);
    }
    doCurdate() {
        this._type = this.determineFunction(MySQLLexer.CURDATE_SYMBOL);
    }
    doCurrentDate() {
        this._type = this.determineFunction(MySQLLexer.CURDATE_SYMBOL);
    }
    doCurrentTime() {
        this._type = this.determineFunction(MySQLLexer.CURTIME_SYMBOL);
    }
    doCurtime() {
        this._type = this.determineFunction(MySQLLexer.CURTIME_SYMBOL);
    }
    doDateAdd() {
        this._type = this.determineFunction(MySQLLexer.DATE_ADD_SYMBOL);
    }
    doDateSub() {
        this._type = this.determineFunction(MySQLLexer.DATE_SUB_SYMBOL);
    }
    doExtract() {
        this._type = this.determineFunction(MySQLLexer.EXTRACT_SYMBOL);
    }
    doGroupConcat() {
        this._type = this.determineFunction(MySQLLexer.GROUP_CONCAT_SYMBOL);
    }
    doMax() {
        this._type = this.determineFunction(MySQLLexer.MAX_SYMBOL);
    }
    doMid() {
        this._type = this.determineFunction(MySQLLexer.SUBSTRING_SYMBOL);
    }
    doMin() {
        this._type = this.determineFunction(MySQLLexer.MIN_SYMBOL);
    }
    doNot() {
        this._type = this.isSqlModeActive(SqlMode.HighNotPrecedence) ? MySQLLexer.NOT2_SYMBOL : MySQLLexer.NOT_SYMBOL;
    }
    doNow() {
        this._type = this.determineFunction(MySQLLexer.NOW_SYMBOL);
    }
    doPosition() {
        this._type = this.determineFunction(MySQLLexer.POSITION_SYMBOL);
    }
    doSessionUser() {
        this._type = this.determineFunction(MySQLLexer.USER_SYMBOL);
    }
    doStddevSamp() {
        this._type = this.determineFunction(MySQLLexer.STDDEV_SAMP_SYMBOL);
    }
    doStddev() {
        this._type = this.determineFunction(MySQLLexer.STD_SYMBOL);
    }
    doStddevPop() {
        this._type = this.determineFunction(MySQLLexer.STD_SYMBOL);
    }
    doStd() {
        this._type = this.determineFunction(MySQLLexer.STD_SYMBOL);
    }
    doSubdate() {
        this._type = this.determineFunction(MySQLLexer.SUBDATE_SYMBOL);
    }
    doSubstr() {
        this._type = this.determineFunction(MySQLLexer.SUBSTRING_SYMBOL);
    }
    doSubstring() {
        this._type = this.determineFunction(MySQLLexer.SUBSTRING_SYMBOL);
    }
    doSum() {
        this._type = this.determineFunction(MySQLLexer.SUM_SYMBOL);
    }
    doSysdate() {
        this._type = this.determineFunction(MySQLLexer.SYSDATE_SYMBOL);
    }
    doSystemUser() {
        this._type = this.determineFunction(MySQLLexer.USER_SYMBOL);
    }
    doTrim() {
        this._type = this.determineFunction(MySQLLexer.TRIM_SYMBOL);
    }
    doVariance() {
        this._type = this.determineFunction(MySQLLexer.VARIANCE_SYMBOL);
    }
    doVarPop() {
        this._type = this.determineFunction(MySQLLexer.VARIANCE_SYMBOL);
    }
    doVarSamp() {
        this._type = this.determineFunction(MySQLLexer.VAR_SAMP_SYMBOL);
    }
    doUnderscoreCharset() {
        this._type = this.checkCharset(this.text);
    }
    isVersionComment() {
        return this.checkMySQLVersion(this.text);
    }
    isBackTickQuotedId() {
        return !this.isSqlModeActive(SqlMode.NoBackslashEscapes);
    }
    isDoubleQuotedText() {
        return !this.isSqlModeActive(SqlMode.NoBackslashEscapes);
    }
    isSingleQuotedText() {
        return !this.isSqlModeActive(SqlMode.NoBackslashEscapes);
    }
    startInVersionComment() {
        this.inVersionComment = true;
    }
    endInVersionComment() {
        this.inVersionComment = false;
    }
    isInVersionComment() {
        return this.inVersionComment;
    }
}
_a = MySQLLexerBase;
_MySQLLexerBase_longString = { value: "2147483647" };
_MySQLLexerBase_longLength = { value: 10 };
_MySQLLexerBase_signedLongString = { value: "-2147483648" };
_MySQLLexerBase_longLongString = { value: "9223372036854775807" };
_MySQLLexerBase_longLongLength = { value: 19 };
_MySQLLexerBase_signedLongLongString = { value: "-9223372036854775808" };
_MySQLLexerBase_signedLongLongLength = { value: 19 };
_MySQLLexerBase_unsignedLongLongString = { value: "18446744073709551615" };
_MySQLLexerBase_unsignedLongLongLength = { value: 20 };
export default MySQLLexerBase;
