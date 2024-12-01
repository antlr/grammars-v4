import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:core';
import 'dart:convert';
import 'dart:collection';
import 'MySQLLexer.dart';
import 'MySQLParser.dart';
import 'SqlMode.dart';
import 'SqlModes.dart';

/** Base lexer class providing functions needed in actions. */
abstract class MySQLLexerBase extends Lexer
{
    int serverVersion = 0;
    HashSet<SqlMode> sqlModes = HashSet<SqlMode>();
    bool supportMle = true;
    HashSet<String> charSets = HashSet<String>();
    bool inVersionComment = false;
    Queue<Token> pendingTokens = Queue<Token>();

    static const String longString = "2147483647";
    static const int longLength = 10;
    static const String signedLongString = "-2147483648";
    static const String longLongString = "9223372036854775807";
    static const int longLongLength = 19;
    static const String signedLongLongString = "-9223372036854775808";
    static const int signedLongLongLength = 19;
    static const String unsignedLongLongString = "18446744073709551615";
    static const int unsignedLongLongLength = 20;

    bool justEmitedDot = false;

    MySQLLexerBase(CharStream input) : super(input)
    {
        this.serverVersion = 80200;
        this.sqlModes = SqlModes.sqlModeFromString("ANSI_QUOTES");
    }

    bool isSqlModeActive(SqlMode mode) {
        return sqlModes.contains(mode);
    }

    /**
     * Resets the lexer by setting initial values to transient member, resetting the input stream position etc.
     */
    @override void reset([bool resetInput = false])
    {
        inVersionComment = false;
        super.reset(true); // reset() needs to follow the semantics in other targets!
    }

    /**
     * Implements the multi token feature required in our lexer.
     * A lexer rule can emit more than a single token, if needed.
     *
     * @returns The next token in the token stream.
     */
    @override Token nextToken()
    {
        // First respond with pending tokens to the next token request, if there are any.
        if (! this.pendingTokens.isEmpty) {
            var pending = this.pendingTokens.removeFirst();
            return pending;
        }

        // Let the main lexer class run the next token recognition.
        // This might create additional tokens again.
        var next = super.nextToken();

        if (! this.pendingTokens.isEmpty) {
            var pending = this.pendingTokens.removeFirst();
            this.pendingTokens.add(next);
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
    bool checkMySQLVersion(String text) {
        if (text.length < 8) {
            return false;
        }

        int version = int.parse(text.substring(3));
        if (version <= serverVersion) {
            inVersionComment = true;
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
    int determineFunction(int proposed) {
        var input = this.inputStream.LA(1) ?? 0;
        if (isSqlModeActive(SqlMode.ignoreSpace)) {
            while ([' ', '\t', '\r', '\n'].contains(String.fromCharCode(input))) {
                // Consume logic based on InputStream equivalent.
                this.interpreter?.consume(this.inputStream);
                this.channel = Lexer.HIDDEN;
                this.type = MySQLLexer.TOKEN_WHITESPACE;
                var c = this.inputStream.LA(1);
                input = c ?? 0;
            }
        }
        var r = input == '('.codeUnitAt(0) ? proposed : MySQLLexer.TOKEN_IDENTIFIER;
        return r;
    }

    /**
     * Checks the given text and determines the smallest number type from it. Code has been taken from sql_lex.cc.
     *
     * @param text The text to parse (which must be a number).
     *
     * @returns The token type for that text.
     */
    int determineNumericType(String text)
    {
        // The original code checks for leading +/- but actually that can never happen, neither in the
        // server parser (as a digit is used to trigger processing in the lexer) nor in our parser
        // as our rules are defined without signs. But we do it anyway for maximum compatibility.
        var length = text.length - 1;
        if (length < MySQLLexerBase.longLength) { // quick normal case
            return MySQLLexer.TOKEN_INT_NUMBER;
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
            return MySQLLexer.TOKEN_INT_NUMBER;
        }

        int smaller;
        int bigger;
        String cmp;
        if (negative) {
            if (length == MySQLLexerBase.longLength) {
                cmp = MySQLLexerBase.signedLongString.substring(1);
                smaller = MySQLLexer.TOKEN_INT_NUMBER; // If <= signed_long_str
                bigger = MySQLLexer.TOKEN_LONG_NUMBER; // If >= signed_long_str
            } else if (length < MySQLLexerBase.signedLongLongLength) {
                return MySQLLexer.TOKEN_LONG_NUMBER;
            } else if (length > MySQLLexerBase.signedLongLongLength) {
                return MySQLLexer.TOKEN_DECIMAL_NUMBER;
            } else {
                cmp = MySQLLexerBase.signedLongLongString.substring(1);
                smaller = MySQLLexer.TOKEN_LONG_NUMBER; // If <= signed_longlong_str
                bigger = MySQLLexer.TOKEN_DECIMAL_NUMBER;
            }
        } else {
            if (length == MySQLLexerBase.longLength) {
                cmp = MySQLLexerBase.longString;
                smaller = MySQLLexer.TOKEN_INT_NUMBER;
                bigger = MySQLLexer.TOKEN_LONG_NUMBER;
            } else if (length < MySQLLexerBase.longLongLength) {
                return MySQLLexer.TOKEN_LONG_NUMBER;
            } else if (length > MySQLLexerBase.longLongLength) {
                if (length > MySQLLexerBase.unsignedLongLongLength) {
                    return MySQLLexer.TOKEN_DECIMAL_NUMBER;
                }
                cmp = MySQLLexerBase.unsignedLongLongString;
                smaller = MySQLLexer.TOKEN_ULONGLONG_NUMBER;
                bigger = MySQLLexer.TOKEN_DECIMAL_NUMBER;
            } else {
                cmp = MySQLLexerBase.longLongString;
                smaller = MySQLLexer.TOKEN_LONG_NUMBER;
                bigger = MySQLLexer.TOKEN_ULONGLONG_NUMBER;
            }
        }

        var otherIndex = 0;
        while (index < text.length && cmp[otherIndex++] == text[index++]) {
            //
        }

        var i = text[index - 1].compareTo(cmp[otherIndex - 1]);
        return i <= 0 ? smaller : bigger;
    }

    /**
     * Checks if the given text corresponds to a charset defined in the server (text is preceded by an underscore).
     *
     * @param text The text to check.
     *
     * @returns UNDERSCORE_CHARSET if so, otherwise IDENTIFIER.
     */
    int checkCharset(String text)
    {
        var z = this.charSets.contains(text);
        var r = z ? MySQLLexer.TOKEN_UNDERSCORE_CHARSET : MySQLLexer.TOKEN_IDENTIFIER;
        return r;
    }

    /**
     * Creates a DOT token in the token stream.
     */
    void emitDot()
    {
        var ctf = this.tokenFactory;
        Token t = ctf.create(
            MySQLLexer.TOKEN_DOT_SYMBOL,
            this.text,
            Pair<TokenSource?, CharStream?>(this, this.inputStream),
            this.channel,
            this.tokenStartCharIndex,
            this.tokenStartCharIndex,
            this.line,
            this.charPositionInLine);
        this.pendingTokens.add(t);
        ++this.charPositionInLine;
        ++this.tokenStartCharIndex;
        this.justEmitedDot = true;
    }

    // Version-related methods
    bool isServerVersionLt80024() => serverVersion < 80024;
    bool isServerVersionGe80024() => serverVersion >= 80024;
    bool isServerVersionGe80011() => serverVersion >= 80011;
    bool isServerVersionGe80013() => serverVersion >= 80013;
    bool isServerVersionLt80014() => serverVersion < 80014;
    bool isServerVersionGe80014() => serverVersion >= 80014;
    bool isServerVersionGe80017() => serverVersion >= 80017;
    bool isServerVersionGe80018() => serverVersion >= 80018;

    bool isMasterCompressionAlgorithm() => serverVersion >= 80018 && isServerVersionLt80024();

    bool isServerVersionLt80031() => serverVersion < 80031;

    // Functions for specific token types
    void doLogicalOr()
    {
        this.type = isSqlModeActive(SqlMode.pipesAsConcat) ? MySQLLexer.TOKEN_CONCAT_PIPES_SYMBOL : MySQLLexer.TOKEN_LOGICAL_OR_OPERATOR;
    }

    void doIntNumber()
    {
        this.type = determineNumericType(this.text);
    }

    void doAdddate() => this.type = determineFunction(MySQLLexer.TOKEN_ADDDATE_SYMBOL);
    void doBitAnd() => this.type = determineFunction(MySQLLexer.TOKEN_BIT_AND_SYMBOL);
    void doBitOr() => this.type = determineFunction(MySQLLexer.TOKEN_BIT_OR_SYMBOL);
    void doBitXor() => this.type = determineFunction(MySQLLexer.TOKEN_BIT_XOR_SYMBOL);
    void doCast() => this.type = determineFunction(MySQLLexer.TOKEN_CAST_SYMBOL);
    void doCount() => this.type = determineFunction(MySQLLexer.TOKEN_COUNT_SYMBOL);
    void doCurdate() => this.type = determineFunction(MySQLLexer.TOKEN_CURDATE_SYMBOL);
    void doCurrentDate() => this.type = determineFunction(MySQLLexer.TOKEN_CURDATE_SYMBOL);
    void doCurrentTime() => this.type = determineFunction(MySQLLexer.TOKEN_CURTIME_SYMBOL);
    void doCurtime() => this.type = determineFunction(MySQLLexer.TOKEN_CURTIME_SYMBOL);
    void doDateAdd() => this.type = determineFunction(MySQLLexer.TOKEN_DATE_ADD_SYMBOL);
    void doDateSub() => this.type = determineFunction(MySQLLexer.TOKEN_DATE_SUB_SYMBOL);
    void doExtract() => this.type = determineFunction(MySQLLexer.TOKEN_EXTRACT_SYMBOL);
    void doGroupConcat() => this.type = determineFunction(MySQLLexer.TOKEN_GROUP_CONCAT_SYMBOL);
    void doMax() => this.type = determineFunction(MySQLLexer.TOKEN_MAX_SYMBOL);
    void doMid() => this.type = determineFunction(MySQLLexer.TOKEN_SUBSTRING_SYMBOL);
    void doMin() => this.type = determineFunction(MySQLLexer.TOKEN_MIN_SYMBOL);
    void doNot() => this.type = isSqlModeActive(SqlMode.highNotPrecedence) ? MySQLLexer.TOKEN_NOT2_SYMBOL : MySQLLexer.TOKEN_NOT_SYMBOL;
    void doNow() => this.type = determineFunction(MySQLLexer.TOKEN_NOW_SYMBOL);
    void doPosition() => this.type = determineFunction(MySQLLexer.TOKEN_POSITION_SYMBOL);
    void doSessionUser() => this.type = determineFunction(MySQLLexer.TOKEN_USER_SYMBOL);
    void doStddevSamp() => this.type = determineFunction(MySQLLexer.TOKEN_STDDEV_SAMP_SYMBOL);
    void doStddev() => this.type = determineFunction(MySQLLexer.TOKEN_STD_SYMBOL);
    void doStddevPop() => this.type = determineFunction(MySQLLexer.TOKEN_STD_SYMBOL);
    void doStd() => this.type = determineFunction(MySQLLexer.TOKEN_STD_SYMBOL);
    void doSubdate() => this.type = determineFunction(MySQLLexer.TOKEN_SUBDATE_SYMBOL);
    void doSubstr() => this.type = determineFunction(MySQLLexer.TOKEN_SUBSTRING_SYMBOL);
    void doSubstring() => this.type = determineFunction(MySQLLexer.TOKEN_SUBSTRING_SYMBOL);
    void doSum() => this.type = determineFunction(MySQLLexer.TOKEN_SUM_SYMBOL);
    void doSysdate() => this.type = determineFunction(MySQLLexer.TOKEN_SYSDATE_SYMBOL);
    void doSystemUser() => this.type = determineFunction(MySQLLexer.TOKEN_USER_SYMBOL);
    void doTrim() => this.type = determineFunction(MySQLLexer.TOKEN_TRIM_SYMBOL);
    void doVariance() => this.type = determineFunction(MySQLLexer.TOKEN_VARIANCE_SYMBOL);
    void doVarPop() => this.type = determineFunction(MySQLLexer.TOKEN_VARIANCE_SYMBOL);
    void doVarSamp() => this.type = determineFunction(MySQLLexer.TOKEN_VAR_SAMP_SYMBOL);
    void doUnderscoreCharset() => this.type = checkCharset(this.text);

    bool isVersionComment() => checkMySQLVersion(this.text);

    bool isBackTickQuotedId()
    {
        return !this.isSqlModeActive(SqlMode.noBackslashEscapes);
    }

    bool isDoubleQuotedText()
    {
        return !this.isSqlModeActive(SqlMode.noBackslashEscapes);
    }

    bool isSingleQuotedText()
    {
        return !this.isSqlModeActive(SqlMode.noBackslashEscapes);
    }

/*
    @override Token emit()
    {
        var ctf = this.tokenFactory;
        Token t = ctf.create(
            this.type,
            (this.text!=null?(this.justEmitedDot?this.text.substring(1):this.text):null),
            Pair<TokenSource?, CharStream?>(this, this.inputStream),
            this.channel,
            this.tokenStartCharIndex + (this.justEmitedDot?1:0),
            CharIndex - 1, 
            this.line,
            this.charPositionInLine);
        this.justEmitedDot = false;
        super.emit(t);
        return t;
    }
*/

    void startInVersionComment()
    {
        inVersionComment = true;
    }

    void endInVersionComment()
    {
        inVersionComment = false;
    }

    bool isInVersionComment()
    {
        return inVersionComment;
    }
}
