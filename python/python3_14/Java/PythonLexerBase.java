/*
The MIT License (MIT)
Copyright (c) 2021 Robert Einhorn

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
 */

/*
 *
 * Project : A helper class for an ANTLR4 Python lexer grammar that assists in tokenizing indentation,
 *           interpolated strings, and encoding declaration.
 *
 * Developed by : Robert Einhorn, robert.einhorn.hu@gmail.com
 *
 */

// ****  Implemented in Java 8 for compatibility with ANTLR4 Java runtime ****

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.Map;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.Pair;

public abstract class PythonLexerBase extends Lexer {
    private static final Map<String, Integer> LEXER_MODES_FOR_ISTRING_START = new HashMap<>();

    private static final int INVALID_LENGTH = -1;
    private static final String ERR_TXT = " ERROR: ";
    private static final int TAB_LENGTH = 8;

    private String encodingName;

    // Indentation handling
    private Deque<Integer> indentLengthStack;
    private Deque<Token> pendingTokens;

    private int previousPendingTokenType;
    private int lastPendingTokenTypeFromDefaultChannel;

    // Parenthesis / bracket / brace counts
    private int opened;
    private Deque<Integer> parenOrBracketOpenedStack;
    private Deque<String> braceExpressionStack;
    private String prevBraceExpression;

    // Current interpolated STRING_MIDDLE token type (FSTRING_MIDDLE or TSTRING_MIDDLE)
    private int curISTRING_MIDDLEtokenType;

    // We reimplement mode/stack because not all runtimes expose _mode/_modeStack
    private int curLexerMode;
    private Deque<Integer> lexerModeStack;

    // Indentation diagnostics
    private boolean wasSpaceIndentation;
    private boolean wasTabIndentation;
    private boolean wasIndentationMixedWithSpacesAndTabs;

    // Current / lookahead tokens
    private Token curToken;
    private Token ffgToken;

    protected PythonLexerBase(CharStream input) {
        super(input);
        this.init();
    }

    @Override
    public void reset() {
        this.init();
        super.reset();
    }

    private void init() {
        this.encodingName = "";
        this.indentLengthStack = new ArrayDeque<>();
        this.pendingTokens = new ArrayDeque<>();
        this.previousPendingTokenType = 0;
        this.lastPendingTokenTypeFromDefaultChannel = 0;
        this.opened = 0;
        this.parenOrBracketOpenedStack = new ArrayDeque<>();
        this.braceExpressionStack = new ArrayDeque<>();
        this.prevBraceExpression = "";
        this.curISTRING_MIDDLEtokenType = 0;
        this.curLexerMode = Lexer.DEFAULT_MODE;
        this.lexerModeStack = new ArrayDeque<>();
        this.wasSpaceIndentation = false;
        this.wasTabIndentation = false;
        this.wasIndentationMixedWithSpacesAndTabs = false;
        this.curToken = null;
        this.ffgToken = null;
    }

    /**
     * Sets the encoding name to emit an ENCODING token at the start of the token stream.
     * Leave empty if not needed (e.g., when parsing from string).
     *
     * @param encodingName the encoding name (e.g., "utf-8"), or empty string to disable ENCODING token
     */
    public void setEncodingName(final String encodingName) {
        this.encodingName = encodingName;
    }

    @Override
    public Token nextToken() { // Reading the input stream until EOF is reached
        this.checkNextToken();
        return this.pendingTokens.pollFirst(); // Add the queued token to the token stream
    }

    private void checkNextToken() {
        if (this.previousPendingTokenType == Token.EOF) return;

        this.setCurrentAndFollowingTokens();
        if (this.indentLengthStack.isEmpty()) { // We're at the first token
            this.handleStartOfInput();
        }

        switch (this.curToken.getType()) {
            case PythonLexer.NEWLINE:
                this.handleNEWLINEtoken();
                break;
            case PythonLexer.LPAR:
            case PythonLexer.LSQB:
            case PythonLexer.LBRACE:
                this.opened++;
                this.addPendingToken(this.curToken);
                break;
            case PythonLexer.RPAR:
            case PythonLexer.RSQB:
            case PythonLexer.RBRACE:
                this.opened--;
                this.addPendingToken(this.curToken);
                break;
            case PythonLexer.FSTRING_MIDDLE:
            case PythonLexer.TSTRING_MIDDLE:
                this.handleISTRING_MIDDLEtokenWithDoubleBrace(); // does not affect the opened field
                this.addPendingToken(this.curToken);
                break;
            case PythonLexer.COLONEQUAL:
                this.handleCOLONEQUALtokenInIString();
                break;
            case PythonLexer.ERRORTOKEN:
                this.reportLexerError("token recognition error at: '" + this.curToken.getText() + "'");
                this.addPendingToken(this.curToken);
                break;
            case Token.EOF:
                this.handleEOFtoken();
                break;
            default:
                this.addPendingToken(this.curToken);
        }
        this.handleFORMAT_SPECIFICATION_MODE();
    }

    private void setCurrentAndFollowingTokens() {
        this.curToken = this.ffgToken == null ?
                        super.nextToken() :
                        this.ffgToken;

        this.checkCurToken(); // Do not use ffgToken in this method or any of its submethods — it hasn't been set yet!

        this.ffgToken = this.curToken.getType() == Token.EOF ?
                        this.curToken :
                        super.nextToken();
    }

    // - initialize indent stack
    // - skip BOM token
    // - insert ENCODING token (if any)
    // - hide leading NEWLINE(s)
    // - insert leading INDENT if first statement is indented
    private void handleStartOfInput() {
        this.indentLengthStack.push(0); // this will never be popped off

        if (this.curToken.getType() == PythonLexer.BOM) {
            this.setCurrentAndFollowingTokens();
        }
        this.insertENCODINGtoken();

        while (this.curToken.getType() != Token.EOF) {
            if (this.curToken.getChannel() == Token.DEFAULT_CHANNEL) {
                if (this.curToken.getType() == PythonLexer.NEWLINE) {
                    // all the NEWLINE tokens must be ignored before the first statement
                    this.hideAndAddPendingToken(this.curToken);
                } else { // We're at the first statement
                    this.insertLeadingIndentToken();
                    return; // continue the processing of the current token with checkNextToken()
                }
            } else {
                this.addPendingToken(this.curToken); // it can be WS, EXPLICIT_LINE_JOINING or COMMENT token
            }
            this.setCurrentAndFollowingTokens();
        }
        // continue the processing of the EOF token with checkNextToken()
    }

    private void insertENCODINGtoken() { // https://peps.python.org/pep-0263/
        if (this.encodingName.isEmpty()) return;

        final Pair<TokenSource, CharStream> sourcePair = this._tokenFactorySourcePair;
        final CommonToken encodingToken = new CommonToken(sourcePair, PythonLexer.ENCODING, Token.HIDDEN_CHANNEL, 0, 0);
        encodingToken.setText(this.encodingName);
        encodingToken.setLine(0);
        encodingToken.setCharPositionInLine(-1);
        this.addPendingToken(encodingToken);
    }

    private void insertLeadingIndentToken() {
        if (this.previousPendingTokenType == PythonLexer.WS) {
            Token prevToken = this.pendingTokens.peekLast(); // WS token
            if (this.getIndentationLength(prevToken.getText()) != 0) { // there is an "indentation" before the first statement
                final String errMsg = "first statement indented";
                this.reportLexerError(errMsg);
                // insert an INDENT token before the first statement to trigger an 'unexpected indent' error later in the parser
                this.createAndAddPendingToken(PythonLexer.INDENT, ERR_TXT + errMsg, this.curToken);
            }
        }
    }

    private void handleNEWLINEtoken() {
        if (!this.lexerModeStack.isEmpty()) { // for multi line f/t-string literals
            this.addPendingToken(this.curToken);
            return;
        }

        if (this.opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
            this.hideAndAddPendingToken(this.curToken);
            return;
        }

        final Token nlToken = new CommonToken(this.curToken); // save the current NEWLINE token
        final boolean isLookingAhead = this.ffgToken.getType() == PythonLexer.WS;
        if (isLookingAhead) {
            this.setCurrentAndFollowingTokens(); // set the next two tokens
        }

        switch (this.ffgToken.getType()) {
            case PythonLexer.NEWLINE: // We're before a blank line
            case PythonLexer.COMMENT: // We're before a comment
                this.hideAndAddPendingToken(nlToken);
                if (isLookingAhead) {
                    this.addPendingToken(this.curToken); // WS token
                }
                break;
            default:
                this.addPendingToken(nlToken);
                if (isLookingAhead) { // We're on a whitespace(s) followed by a statement
                    final int indentationLength = this.ffgToken.getType() == Token.EOF ?
                                                  0 :
                                                  this.getIndentationLength(this.curToken.getText());

                    if (indentationLength != PythonLexerBase.INVALID_LENGTH) {
                        this.addPendingToken(this.curToken); // WS token
                        this.insertIndentOrDedentToken(indentationLength); // may insert INDENT token or DEDENT token(s)
                    } else {
                        this.reportError("inconsistent use of tabs and spaces in indentation");
                    }
                } else { // We're at a newline followed by a statement (there is no whitespace before the statement)
                    this.insertIndentOrDedentToken(0); // may insert DEDENT token(s)
                }
        }
    }

    private void insertIndentOrDedentToken(final int indentLength) {
        int prevIndentLength = this.indentLengthStack.peek();
        if (indentLength > prevIndentLength) {
            this.createAndAddPendingToken(PythonLexer.INDENT, null, this.ffgToken);
            this.indentLengthStack.push(indentLength);
            return;
        }

        while (indentLength < prevIndentLength) { // more than 1 DEDENT token may be inserted to the token stream
            this.indentLengthStack.pop();
            prevIndentLength = this.indentLengthStack.peek();
            if (indentLength <= prevIndentLength) {
                this.createAndAddPendingToken(PythonLexer.DEDENT, null, this.ffgToken);
            } else {
                this.reportError("inconsistent dedent");
            }
        }
    }

    private void checkCurToken() {
        switch (this.curToken.getType()) {
            case PythonLexer.FSTRING_START:
                this.curISTRING_MIDDLEtokenType = PythonLexer.FSTRING_MIDDLE;
                this.setLexerModeByISTRING_STARTtoken();
                return;
            case PythonLexer.TSTRING_START:
                this.curISTRING_MIDDLEtokenType = PythonLexer.TSTRING_MIDDLE;
                this.setLexerModeByISTRING_STARTtoken();
                return;
            case PythonLexer.FSTRING_MIDDLE:
            case PythonLexer.TSTRING_MIDDLE:
                this.handleISTRING_MIDDLEtokenWithQuoteAndLBrace(); // affect the opened field
                switch (this.curToken.getType()) {
                    case PythonLexer.FSTRING_MIDDLE:
                    case PythonLexer.TSTRING_MIDDLE:
                        return; // No curToken exchange happened
                }
                break;
            case PythonLexer.FSTRING_END:
            case PythonLexer.TSTRING_END:
                this.popLexerMode();
                return;
            default:
                if (this.lexerModeStack.isEmpty()) {
                    return; // Not in f/t-string mode
                }
        }
        this.processBraceExpression();
    }

    private void processBraceExpression() {
        switch (this.curToken.getType()) { // the following tokens can only come from default mode (after an LBRACE in f/t-string)
            case PythonLexer.NEWLINE:
                // append the current brace expression with the current newline
                this.appendToBraceExpression(this.curToken.getText());
                final CommonToken nlToken = new CommonToken(this.curToken);
                nlToken.setChannel(Token.HIDDEN_CHANNEL);
                this.curToken = nlToken;
                break;
            case PythonLexer.LBRACE:
                // the outermost brace expression cannot be a dictionary comprehension or a set comprehension
                this.braceExpressionStack.push("{");
                this.parenOrBracketOpenedStack.push(0);
                this.pushLexerMode(Lexer.DEFAULT_MODE);
                break;
            case PythonLexer.LPAR:
            case PythonLexer.LSQB:
                // append the current brace expression with a "(" or a "["
                this.appendToBraceExpression(this.curToken.getText());
                // https://peps.python.org/pep-0498/#lambdas-inside-expressions
                this.incrementBraceStack();
                break;
            case PythonLexer.RPAR:
            case PythonLexer.RSQB:
                // append the current brace expression with a ")" or a "]"
                this.appendToBraceExpression(this.curToken.getText());
                this.decrementBraceStack();
                break;
            case PythonLexer.COLON:
            case PythonLexer.COLONEQUAL:
                // append the current brace expression with a ":" or a ":="
                this.appendToBraceExpression(this.curToken.getText());
                this.setLexerModeByCOLONorCOLONEQUALtoken();
                break;
            case PythonLexer.RBRACE:
                this.setLexerModeAfterRBRACEtoken();
                break;
            default:
                // append the current brace expression with the current token text
                this.appendToBraceExpression(this.curToken.getText());
        }
    }

    private void appendToBraceExpression(final String text) {
        final String top = this.braceExpressionStack.pop();
        this.braceExpressionStack.push(top + text);
    }

    private void incrementBraceStack() { // increment the last element
        this.parenOrBracketOpenedStack.push(this.parenOrBracketOpenedStack.pop() + 1);
    }

    private void decrementBraceStack() { // decrement the last element
        this.parenOrBracketOpenedStack.push(this.parenOrBracketOpenedStack.pop() - 1);
    }

    private void setLexerModeAfterRBRACEtoken() {
        switch (this.curLexerMode) {
            case Lexer.DEFAULT_MODE:
                this.popLexerMode();
                this.popByBRACE();
                break;
            case PythonLexer.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.popLexerMode();
                this.popLexerMode();
                this.popByBRACE();
                break;
            default:
                this.reportLexerError("f-string: single '}' is not allowed");
        }
    }

    private void setLexerModeByISTRING_STARTtoken() { // ISTRING = interpolated string (FSTRING or TSTRING)
        if (PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.isEmpty()) {
            PythonLexerBase.initLexerModesForIStringStart();
        }

        final String interpolatedStringPrefix = this.curToken.getText().toLowerCase();
        final Integer newLexerMode = PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.get(interpolatedStringPrefix);
        if (newLexerMode != null) {
            this.pushLexerMode(newLexerMode);
        } else {
            this.reportLexerError("internal error: unknown interpolated string literal prefix: " + this.curToken.getText());
        }
    }

    private static void initLexerModesForIStringStart() {
        // f-strings
        LEXER_MODES_FOR_ISTRING_START.put("f'", PythonLexer.SQ1__FSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("rf'", PythonLexer.SQ1R_FSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("fr'", PythonLexer.SQ1R_FSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("f\"", PythonLexer.DQ1__FSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("rf\"", PythonLexer.DQ1R_FSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("fr\"", PythonLexer.DQ1R_FSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("f'''", PythonLexer.SQ3__FSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("rf'''", PythonLexer.SQ3R_FSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("fr'''", PythonLexer.SQ3R_FSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("f\"\"\"", PythonLexer.DQ3__FSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("rf\"\"\"", PythonLexer.DQ3R_FSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("fr\"\"\"", PythonLexer.DQ3R_FSTRING_MODE);

        // t-strings
        LEXER_MODES_FOR_ISTRING_START.put("t'", PythonLexer.SQ1__TSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("rt'", PythonLexer.SQ1R_TSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("tr'", PythonLexer.SQ1R_TSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("t\"", PythonLexer.DQ1__TSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("rt\"", PythonLexer.DQ1R_TSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("tr\"", PythonLexer.DQ1R_TSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("t'''", PythonLexer.SQ3__TSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("rt'''", PythonLexer.SQ3R_TSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("tr'''", PythonLexer.SQ3R_TSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("t\"\"\"", PythonLexer.DQ3__TSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("rt\"\"\"", PythonLexer.DQ3R_TSTRING_MODE);
        LEXER_MODES_FOR_ISTRING_START.put("tr\"\"\"", PythonLexer.DQ3R_TSTRING_MODE);
    }

    private void setLexerModeByCOLONorCOLONEQUALtoken() {
        // Exit early when the current lexer mode indicates an open parenthesis/bracket
        if (this.parenOrBracketOpenedStack.peek() != 0) {
            return;
        }

        // COLONEQUAL token will be replaced with a COLON token in checkNextToken()
        final int prevLexerMode = lexerModeStack.peek();
        switch (prevLexerMode) { // check the previous lexer mode (the current is DEFAULT_MODE)
            case PythonLexer.SQ1__FSTRING_MODE:
            case PythonLexer.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.SQ1__TSTRING_MODE:
            case PythonLexer.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.SQ1R_FSTRING_MODE:
            case PythonLexer.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.SQ1R_TSTRING_MODE:
            case PythonLexer.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.DQ1__FSTRING_MODE:
            case PythonLexer.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.DQ1__TSTRING_MODE:
            case PythonLexer.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.DQ1R_FSTRING_MODE:
            case PythonLexer.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.DQ1R_TSTRING_MODE:
            case PythonLexer.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.SQ3__FSTRING_MODE:
            case PythonLexer.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.SQ3__TSTRING_MODE:
            case PythonLexer.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.SQ3R_FSTRING_MODE:
            case PythonLexer.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.SQ3R_TSTRING_MODE:
            case PythonLexer.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.DQ3__FSTRING_MODE:
            case PythonLexer.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.DQ3__TSTRING_MODE:
            case PythonLexer.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.DQ3R_FSTRING_MODE:
            case PythonLexer.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
            case PythonLexer.DQ3R_TSTRING_MODE:
            case PythonLexer.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
        }
    }

    private void popByBRACE() {
        this.parenOrBracketOpenedStack.pop();
        String curBraceExpression = this.braceExpressionStack.pop();
        this.prevBraceExpression = curBraceExpression + "}";
        if (!this.braceExpressionStack.isEmpty()) {
            // Extend the current brace expression by adding the previous expression
            curBraceExpression = this.braceExpressionStack.pop();
            this.braceExpressionStack.push(curBraceExpression + this.prevBraceExpression);
        }
    }

    private void handleISTRING_MIDDLEtokenWithDoubleBrace() { // ISTRING = interpolated string (FSTRING or TSTRING)
        // replace the trailing double brace with a single brace and insert a hidden brace token
        final String lastTwoChars = this.getLastTwoCharsOfTheCurTokenText();
        switch (lastTwoChars) {
            case "{{":
                this.trimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", Token.HIDDEN_CHANNEL);
                break;
            case "}}":
                this.trimLastCharAddPendingTokenSetCurToken(PythonLexer.RBRACE, "}", Token.HIDDEN_CHANNEL);
                break;
        }
    }

    private void handleISTRING_MIDDLEtokenWithQuoteAndLBrace() { // ISTRING = interpolated string (FSTRING or TSTRING)
        // replace the trailing     quote + left_brace with a quote     and insert an LBRACE token
        // replace the trailing backslash + left_brace with a backslash and insert an LBRACE token
        final String lastTwoChars = this.getLastTwoCharsOfTheCurTokenText();
        switch (lastTwoChars) {
            case "\"{":
            case "'{":
            case "\\{":
                this.trimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", Token.DEFAULT_CHANNEL);
                break;
        }
    }

    private String getLastTwoCharsOfTheCurTokenText() {
        final String text = this.curToken.getText();
        return text.length() >= 2 ? text.substring(text.length() - 2) : text;
    }

    private void trimLastCharAddPendingTokenSetCurToken(final int type, final String text, final int channel) {
        // trim the last char and add the modified curToken to the pendingTokens stack
        final String curTokenText = this.curToken.getText();
        final String tokenTextWithoutLastChar = curTokenText.substring(0, curTokenText.length() - 1);
        final CommonToken token = new CommonToken(this.curToken);
        token.setText(tokenTextWithoutLastChar);
        token.setStopIndex(token.getStopIndex() - 1);
        this.addPendingToken(token);

        this.createNewCurToken(type, text, channel); // set curToken
    }

    private void handleCOLONEQUALtokenInIString() { // ISTRING = interpolated string (FSTRING or TSTRING)
        if (!this.lexerModeStack.isEmpty() &&
            this.parenOrBracketOpenedStack.peek() == 0) {

            // In an f/t-string, the walrus operator (:=) is only allowed inside parentheses.
            // If used outside, split the COLONEQUAL token into a COLON
            // (used as a format specifier instead of a walrus operator),
            // and move the equal sign to the beginning of the next token (FSTRING_MIDDLE or TSTRING_MIDDLE).
            CommonToken colonequalToken = new CommonToken(this.curToken);
            colonequalToken.setType(PythonLexer.COLON);
            colonequalToken.setText(":");
            colonequalToken.setStopIndex(colonequalToken.getStartIndex());
            this.curToken = colonequalToken;

            switch (this.ffgToken.getType()) {
                case PythonLexer.FSTRING_MIDDLE:
                case PythonLexer.TSTRING_MIDDLE:
                    colonequalToken = new CommonToken(this.ffgToken);
                    colonequalToken.setText("=" + colonequalToken.getText());
                    colonequalToken.setStartIndex(colonequalToken.getStartIndex() - 1);
                    colonequalToken.setCharPositionInLine(colonequalToken.getCharPositionInLine() - 1);
                    this.ffgToken = colonequalToken;
                    break;
                default:
                    this.addPendingToken(this.curToken);
                    this.createNewCurToken(this.curISTRING_MIDDLEtokenType, "=", Token.DEFAULT_CHANNEL);
            }
        }
        this.addPendingToken(this.curToken);
    }

    private void createNewCurToken(final int type, final String text, final int channel) {
        final CommonToken token = new CommonToken(this.curToken);
        token.setType(type);
        token.setText(text);
        token.setChannel(channel);
        token.setCharPositionInLine(token.getCharPositionInLine() + 1);
        token.setStartIndex(token.getStartIndex() + 1);
        token.setStopIndex(token.getStartIndex());
        this.curToken = token;
    }

    private void pushLexerMode(final int mode) {
        this.pushMode(mode);
        this.lexerModeStack.push(this.curLexerMode);
        this.curLexerMode = mode;
    }

    private void popLexerMode() {
        this.popMode();
        this.curLexerMode = this.lexerModeStack.pop();
    }

    private void handleFORMAT_SPECIFICATION_MODE() {
        if (this.lexerModeStack.isEmpty() || this.ffgToken.getType() != PythonLexer.RBRACE) {
            return;
        }

        // insert an empty FSTRING_MIDDLE or TSTRING_MIDDLE token instead of the missing format specification
        switch (this.curToken.getType()) {
            case PythonLexer.COLON:
                this.createAndAddPendingToken(this.curISTRING_MIDDLEtokenType, "", this.ffgToken);
                break;
            case PythonLexer.RBRACE:
                // only when the previous brace expression is not a dictionary comprehension or set comprehension
                if (!isValid_DictionaryOrSet_ComprehensionExpression(this.prevBraceExpression)) {
                    this.createAndAddPendingToken(this.curISTRING_MIDDLEtokenType, "", this.ffgToken);
                }
                break;
            default:
                break;
        }
    }

    private boolean isValid_DictionaryOrSet_ComprehensionExpression(final String code) {
        final CharStream inputStream = CharStreams.fromString(code);
        final PythonLexer lexer = new PythonLexer(inputStream);
        final CommonTokenStream tokenStream = new CommonTokenStream(lexer);
        PythonParser parser = new PythonParser(tokenStream);

        // Disable error listeners to suppress console output
        lexer.removeErrorListeners();
        parser.removeErrorListeners();

        parser.dictcomp(); // Try parsing as dictionary comprehension
        if (parser.getNumberOfSyntaxErrors() == 0)
            return true;

        parser = new PythonParser(tokenStream);
        tokenStream.seek(0);
        parser.removeErrorListeners();
        parser.setcomp(); // Try parsing as set comprehension
        return parser.getNumberOfSyntaxErrors() == 0;
    }

    private void insertTrailingTokens() {
        switch (this.lastPendingTokenTypeFromDefaultChannel) {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default: // insert an extra trailing NEWLINE token that serves as the end of the last statement
                this.createAndAddPendingToken(PythonLexer.NEWLINE, null, this.ffgToken); // ffgToken is EOF
        }
        this.insertIndentOrDedentToken(0); // Now insert as much trailing DEDENT tokens as needed
    }

    private void handleEOFtoken() {
        if (this.lastPendingTokenTypeFromDefaultChannel > 0) {
            // there was a statement in the input (leading NEWLINE tokens are hidden)
            this.insertTrailingTokens();
        }
        this.addPendingToken(this.curToken);
    }

    private void hideAndAddPendingToken(final Token originalToken) {
        final CommonToken token = new CommonToken(originalToken);
        token.setChannel(Token.HIDDEN_CHANNEL);
        this.addPendingToken(token);
    }

    private void createAndAddPendingToken(final int tokenType, final String text, final Token originalToken) {
        final CommonToken token = new CommonToken(originalToken);
        token.setType(tokenType);
        token.setChannel(Token.DEFAULT_CHANNEL);
        token.setStopIndex(originalToken.getStartIndex() - 1);
        token.setText(text == null ?
                      "<" + this.getVocabulary().getSymbolicName(tokenType) + ">" :
                      text);

        this.addPendingToken(token);
    }

    private void addPendingToken(final Token token) {
        // save the last pending token type because the pendingTokens list can be empty by the nextToken()
        this.previousPendingTokenType = token.getType();
        if (token.getChannel() == Token.DEFAULT_CHANNEL) {
            this.lastPendingTokenTypeFromDefaultChannel = this.previousPendingTokenType;
        }
        this.pendingTokens.addLast(token);
    }

    private int getIndentationLength(final String indentText) { // the indentText may contain spaces, tabs or form feeds
        int length = 0;
        for (char ch : indentText.toCharArray()) {
            switch (ch) {
                case ' ':
                    this.wasSpaceIndentation = true;
                    length += 1;
                    break;
                case '\t':
                    this.wasTabIndentation = true;
                    length += PythonLexerBase.TAB_LENGTH - (length % PythonLexerBase.TAB_LENGTH);
                    break;
                case '\f': // form feed
                    length = 0;
                    break;
            }
        }

        if (this.wasTabIndentation && this.wasSpaceIndentation) {
            if (!(this.wasIndentationMixedWithSpacesAndTabs)) {
                this.wasIndentationMixedWithSpacesAndTabs = true;
                length = PythonLexerBase.INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    private void reportLexerError(final String errMsg) {
        this.getErrorListenerDispatch().syntaxError(this, this.curToken.getType(), this.curToken.getLine(), this.curToken.getCharPositionInLine(), " LEXER" + ERR_TXT + errMsg, null);
    }

    private void reportError(final String errMsg) {
        this.reportLexerError(errMsg);
        this.createAndAddPendingToken(PythonLexer.ERRORTOKEN, ERR_TXT + errMsg, this.ffgToken);
        // the ERRORTOKEN also triggers a parser error
    }
}
