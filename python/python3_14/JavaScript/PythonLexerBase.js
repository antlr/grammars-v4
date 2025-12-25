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
 * Project      : A helper class for an ANTLR4 Python lexer grammar that assists in tokenizing indentation,
 *                interpolated strings, and encoding declaration. 
 *
 * Developed by : Robert Einhorn, robert.einhorn.hu@gmail.com
 *
 */

import { CharStreams, CommonTokenStream, Token, CommonToken, Lexer } from "antlr4";
import PythonLexer from "./PythonLexer.js";
import PythonParser from "./PythonParser.js";

export default class PythonLexerBase extends Lexer {
    static #LEXER_MODES_FOR_ISTRING_START = new Map();
    static #INVALID_LENGTH = -1;
    static #ERR_TXT = " ERROR: ";
    static #TAB_LENGTH = 8;

    #encodingName;

    // Indentation handling
    #indentLengthStack;
    #pendingTokens;

    #previousPendingTokenType;
    #lastPendingTokenTypeFromDefaultChannel;

    // Parenthesis / bracket / brace counts
    #opened;
    #paren_or_bracket_openedStack;
    #braceExpressionStack;
    #prevBraceExpression;

    // Current interpolated STRING_MIDDLE token type (FSTRING_MIDDLE or TSTRING_MIDDLE)
    #curISTRING_MIDDLEtokenType;;

    // We reimplement mode/stack because not all runtimes expose _mode/_modeStack
    #curLexerMode;
    #lexerModeStack;

    // Indentation diagnostics
    #wasSpaceIndentation;
    #wasTabIndentation;
    #wasIndentationMixedWithSpacesAndTabs;

    // Current / lookahead tokens
    #curToken;
    #ffgToken;

    constructor(input) {
        super(input);
        this.#init();
    }

    reset() {
        this.#init();
        super.reset();
    }

    #init() {
        this.#encodingName = "";
        this.#indentLengthStack = [];
        this.#pendingTokens = [];
        this.#previousPendingTokenType = 0;
        this.#lastPendingTokenTypeFromDefaultChannel = 0;
        this.#opened = 0;
        this.#paren_or_bracket_openedStack = [];
        this.#braceExpressionStack = [];
        this.#prevBraceExpression = "";
        this.#curISTRING_MIDDLEtokenType = 0;
        this.#curLexerMode = Lexer.DEFAULT_MODE;
        this.#lexerModeStack = [];
        this.#wasSpaceIndentation = false;
        this.#wasTabIndentation = false;
        this.#wasIndentationMixedWithSpacesAndTabs = false;
        this.#curToken = null;
        this.#ffgToken = null;
    }

    /**
     * Sets the encoding name to emit an ENCODING token at the start of the token stream.
     * Leave empty if not needed (e.g., when parsing from string).
     *
     * @param {string} encodingName - The encoding name (e.g., "utf-8"), or empty string to disable ENCODING token.
     */
    setEncodingName(encodingName) {
        this.encodingName = encodingName;
    }

    nextToken() { // Reading the input stream until EOF is reached
        this.#checkNextToken();
        return this.#pendingTokens.shift() /* stack pollFirst() */; // Add the queued token to the token stream
    }

    #checkNextToken() {
        if (this.#previousPendingTokenType === Token.EOF) {
            return;
        }

        this.#setCurrentAndFollowingTokens();
        if (this.#indentLengthStack.length === 0) { // We're at the first token
            this.#handleStartOfInput();
        }

        switch (this.#curToken.type) {
            case PythonLexer.NEWLINE:
                this.#handleNEWLINEtoken();
                break;
            case PythonLexer.LPAR:
            case PythonLexer.LSQB:
            case PythonLexer.LBRACE:
                this.#opened++;
                this.#addPendingToken(this.#curToken);
                break;
            case PythonLexer.RPAR:
            case PythonLexer.RSQB:
            case PythonLexer.RBRACE:
                this.#opened--;
                this.#addPendingToken(this.#curToken);
                break;
            case PythonLexer.FSTRING_MIDDLE:
            case PythonLexer.TSTRING_MIDDLE:
                this.#handleFSTRING_MIDDLEtokenWithDoubleBrace(); // does not affect the opened field
                this.#addPendingToken(this.#curToken);
                break;
            case PythonLexer.COLONEQUAL:
                this.#handleCOLONEQUALtokenInIString();
                break;
            case PythonLexer.ERRORTOKEN:
                this.#reportLexerError(`token recognition error at: '${this.#curToken.text}'`);
                this.#addPendingToken(this.#curToken);
                break;
            case Token.EOF:
                this.#handleEOFtoken();
                break;
            default:
                this.#addPendingToken(this.#curToken);
        }
        this.#handleFORMAT_SPECIFICATION_MODE();
    }

    #setCurrentAndFollowingTokens() {
        this.#curToken = this.#ffgToken == undefined ?
            super.nextToken() :
            this.#ffgToken;

        this.#checkCurToken(); // Do not use ffgToken in this method or any of its submethods — it hasn't been set yet!

        this.#ffgToken = this.#curToken.type === Token.EOF ?
            this.#curToken :
            super.nextToken();
    }

    // - initialize indent stack
    // - skip BOM token
    // - insert ENCODING token (if any)
    // - hide leading NEWLINE(s)
    // - insert leading INDENT if first statement is indented
    #handleStartOfInput() {
        // initialize the stack with a default 0 indentation length
        this.#indentLengthStack.push(0); // this will never be popped off

        if (this.#curToken.type === PythonLexer.BOM) {
            this.#setCurrentAndFollowingTokens();
        }

        this.#insertENCODINGtoken();

        while (this.#curToken.type !== Token.EOF) {
            if (this.#curToken.channel === Token.DEFAULT_CHANNEL) {
                if (this.#curToken.type === PythonLexer.NEWLINE) {
                    // all the NEWLINE tokens must be ignored before the first statement
                    this.#hideAndAddPendingToken(this.#curToken);
                } else { // We're at the first statement
                    this.#insertLeadingIndentToken();
                    return; // continue the processing of the current token with #checkNextToken()
                }
            } else {
                this.#addPendingToken(this.#curToken); // it can be WS, EXPLICIT_LINE_JOINING or COMMENT token
            }
            this.#setCurrentAndFollowingTokens();
        } // continue the processing of the EOF token with #checkNextToken()
    }

    #insertENCODINGtoken() {
        if (this.#encodingName === "") return

        const sourcePair = [this, this._input];
        const encodingToken = new CommonToken(sourcePair, PythonLexer.ENCODING, Token.HIDDEN_CHANNEL, /*start*/ 0, /*stop*/ 0);
        encodingToken.text = this.#encodingName;
        encodingToken.line = 0;
        encodingToken.column = -1;
        this.#addPendingToken(encodingToken);
    }

    #insertLeadingIndentToken() {
        if (this.#previousPendingTokenType === PythonLexer.WS) {
            const prevToken = this.#pendingTokens.at(-1); /* stack peek */ // WS token
            if (this.#getIndentationLength(prevToken.text) !== 0) { // there is an "indentation" before the first statement
                const errMsg = "first statement indented";
                this.#reportLexerError(errMsg);
                // insert an INDENT token before the first statement to trigger an 'unexpected indent' error later in the parser
                this.#createAndAddPendingToken(PythonLexer.INDENT, PythonLexerBase.#ERR_TXT + errMsg, this.#curToken);
            }
        }
    }

    #handleNEWLINEtoken() {
        if (this.#lexerModeStack.length > 0) { // for multi line f/t-string literals
            this.#addPendingToken(this.#curToken);
            return;
        }

        if (this.#opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
            this.#hideAndAddPendingToken(this.#curToken);
            return;
        }

        const nlToken = this.#curToken.clone(); // save the current NEWLINE token
        const isLookingAhead = this.#ffgToken.type === PythonLexer.WS;
        if (isLookingAhead) {
            this.#setCurrentAndFollowingTokens(); // set the next two tokens
        }

        switch (this.#ffgToken.type) {
            case PythonLexer.NEWLINE: // We're before a blank line
            case PythonLexer.COMMENT: // We're before a comment
                this.#hideAndAddPendingToken(nlToken);
                if (isLookingAhead) {
                    this.#addPendingToken(this.#curToken); // WS token
                }
                break;
            default:
                this.#addPendingToken(nlToken);
                if (isLookingAhead) { // We're on a whitespace(s) followed by a statement
                    const indentationLength = this.#ffgToken.type === Token.EOF ?
                        0 :
                        this.#getIndentationLength(this.#curToken.text);

                    if (indentationLength !== PythonLexerBase.#INVALID_LENGTH) {
                        this.#addPendingToken(this.#curToken); // WS token
                        this.#insertIndentOrDedentToken(indentationLength); // may insert INDENT token or DEDENT token(s)
                    } else {
                        this.#reportError("inconsistent use of tabs and spaces in indentation");
                    }
                } else { // We're at a newline followed by a statement (there is no whitespace before the statement)
                    this.#insertIndentOrDedentToken(0); // may insert DEDENT token(s)
                }
        }
    }

    #insertIndentOrDedentToken(curIndentLength) {
        let prevIndentLength = this.#indentLengthStack.at(-1) /* stack peek */;
        if (curIndentLength > prevIndentLength) {
            this.#createAndAddPendingToken(PythonLexer.INDENT, null, this.#ffgToken);
            this.#indentLengthStack.push(curIndentLength);
            return;
        }

        while (curIndentLength < prevIndentLength) { // more than 1 DEDENT token may be inserted to the token stream
            this.#indentLengthStack.pop();
            prevIndentLength = this.#indentLengthStack.at(-1) /* stack peek */;
            if (curIndentLength <= prevIndentLength) {
                this.#createAndAddPendingToken(PythonLexer.DEDENT, null, this.#ffgToken);
            } else {
                this.#reportError("inconsistent dedent");
            }
        }
    }

    #checkCurToken() {
        switch (this.#curToken.type) {
            case PythonLexer.FSTRING_START:
                this.#curISTRING_MIDDLEtokenType = PythonLexer.FSTRING_MIDDLE;
                this.#setLexerModeByISTRING_STARTtoken();
                return;
            case PythonLexer.TSTRING_START:
                this.#curISTRING_MIDDLEtokenType = PythonLexer.TSTRING_MIDDLE;
                this.#setLexerModeByISTRING_STARTtoken();
                return;
            case PythonLexer.FSTRING_MIDDLE:
            case PythonLexer.TSTRING_MIDDLE:
                this.#handleFSTRING_MIDDLEtokenWithQuoteAndLBrace(); // affect the opened field
                switch (this.#curToken.type) {
                    case PythonLexer.FSTRING_MIDDLE:
                    case PythonLexer.TSTRING_MIDDLE:
                        return; // No curToken exchange happened
                }
                break;
            case PythonLexer.FSTRING_END:
            case PythonLexer.TSTRING_END:
                this.#popLexerMode();
                return;
            default:
                if (this.#lexerModeStack.length === 0) {
                    return; // Not in fstring mode
                }

        }
        this.#processBraceExpression();
    }

    #processBraceExpression() {
        switch (this.#curToken.type) { // the following tokens can only come from default mode (after an LBRACE in f/t-string)
            case PythonLexer.NEWLINE:
                // append the current brace expression with the current newline
                this.#appendToBraceExpression(this.#curToken.text)
                this.#curToken.channel = Token.HIDDEN_CHANNEL;
                break;
            case PythonLexer.LBRACE:
                // the outermost brace expression cannot be a dictionary comprehension or a set comprehension
                this.#braceExpressionStack.push("{");
                this.#paren_or_bracket_openedStack.push(0);
                this.#pushLexerMode(Lexer.DEFAULT_MODE);
                break;
            case PythonLexer.LPAR:
            case PythonLexer.LSQB:
                // append the current brace expression with a "(" or a "["
                this.#appendToBraceExpression(this.#curToken.text)
                // https://peps.python.org/pep-0498/#lambdas-inside-expressions
                this.#incrementBraceStack();
                break;
            case PythonLexer.RPAR:
            case PythonLexer.RSQB:
                // append the current brace expression with a ")" or a "]"
                this.#appendToBraceExpression(this.#curToken.text)
                this.#decrementBraceStack();
                break;
            case PythonLexer.COLON:
            case PythonLexer.COLONEQUAL:
                // append the current brace expression with a ":" or a ":="
                this.#appendToBraceExpression(this.#curToken.text)
                this.#setLexerModeByCOLONorCOLONEQUALtoken();
                break;
            case PythonLexer.RBRACE:
                this.#setLexerModeAfterRBRACEtoken();
                break;
            default:
                // append the current brace expression with the current token text
                this.#appendToBraceExpression(this.#curToken.text)
        }
    }

    #appendToBraceExpression(text) {
        const lastIndex = this.#braceExpressionStack.length - 1;
        this.#braceExpressionStack[lastIndex] += text;
    }

    #incrementBraceStack() { // increment the last element (stack peek + 1)
        const lastIndex = this.#paren_or_bracket_openedStack.length - 1;
        this.#paren_or_bracket_openedStack[lastIndex]++;
    }

    #decrementBraceStack() { // decrement the last element (stack peek - 1)
        const lastIndex = this.#paren_or_bracket_openedStack.length - 1;
        this.#paren_or_bracket_openedStack[lastIndex]--;
    }

    #setLexerModeAfterRBRACEtoken() {
        switch (this.#curLexerMode) {
            case Lexer.DEFAULT_MODE:
                this.#popLexerMode();
                this.#popByBRACE();
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
                this.#popLexerMode();
                this.#popLexerMode();
                this.#popByBRACE();
                break;
            default:
                this.#reportLexerError("f-string: single '}' is not allowed");
        }
    }

    #setLexerModeByISTRING_STARTtoken() { // ISTRING = interpolated string (FSTRING or TSTRING)
        if (PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.size === 0) {
            PythonLexerBase.#initLexerModesForIStringStart();
        }

        const interpolatedStringPrefix = this.#curToken.text.toLowerCase();
        if (PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.has(interpolatedStringPrefix)) {
            const newLexerMode = PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.get(interpolatedStringPrefix);
            this.#pushLexerMode(newLexerMode);
        } else {
            this.#reportLexerError(
                "internal error: unknown interpolated string literal prefix: " + this.#curToken.text
            );
        }
    }

    static #initLexerModesForIStringStart() {
        // f-strings
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("f'", PythonLexer.SQ1__FSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("rf'", PythonLexer.SQ1R_FSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("fr'", PythonLexer.SQ1R_FSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("f\"", PythonLexer.DQ1__FSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("rf\"", PythonLexer.DQ1R_FSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("fr\"", PythonLexer.DQ1R_FSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("f'''", PythonLexer.SQ3__FSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("rf'''", PythonLexer.SQ3R_FSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("fr'''", PythonLexer.SQ3R_FSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("f\"\"\"", PythonLexer.DQ3__FSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("rf\"\"\"", PythonLexer.DQ3R_FSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("fr\"\"\"", PythonLexer.DQ3R_FSTRING_MODE);

        // t-strings
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("t'", PythonLexer.SQ1__TSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("rt'", PythonLexer.SQ1R_TSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("tr'", PythonLexer.SQ1R_TSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("t\"", PythonLexer.DQ1__TSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("rt\"", PythonLexer.DQ1R_TSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("tr\"", PythonLexer.DQ1R_TSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("t'''", PythonLexer.SQ3__TSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("rt'''", PythonLexer.SQ3R_TSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("tr'''", PythonLexer.SQ3R_TSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("t\"\"\"", PythonLexer.DQ3__TSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("rt\"\"\"", PythonLexer.DQ3R_TSTRING_MODE);
        PythonLexerBase.#LEXER_MODES_FOR_ISTRING_START.set("tr\"\"\"", PythonLexer.DQ3R_TSTRING_MODE);
    }

    #setLexerModeByCOLONorCOLONEQUALtoken() {
        // Exit early when the current lexer mode indicates an open parenthesis/bracket
        const opened = this.#paren_or_bracket_openedStack.at(-1) > 0; /* stack peek */
        if (opened) {
            return;
        }

        // COLONEQUAL token will be replaced with a COLON token in CheckNextToken()
        const prevLexerMode = this.#lexerModeStack.at(-1); /* stack peek */
        switch (prevLexerMode) {
            case PythonLexer.SQ1__FSTRING_MODE:
            case PythonLexer.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ1__TSTRING_MODE:
            case PythonLexer.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ1R_FSTRING_MODE:
            case PythonLexer.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ1R_TSTRING_MODE:
            case PythonLexer.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ1__FSTRING_MODE:
            case PythonLexer.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ1__TSTRING_MODE:
            case PythonLexer.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ1R_FSTRING_MODE:
            case PythonLexer.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ1R_TSTRING_MODE:
            case PythonLexer.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ3__FSTRING_MODE:
            case PythonLexer.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ3__TSTRING_MODE:
            case PythonLexer.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ3R_FSTRING_MODE:
            case PythonLexer.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ3R_TSTRING_MODE:
            case PythonLexer.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ3__FSTRING_MODE:
            case PythonLexer.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ3__TSTRING_MODE:
            case PythonLexer.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ3R_FSTRING_MODE:
            case PythonLexer.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ3R_TSTRING_MODE:
            case PythonLexer.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.#pushLexerMode(PythonLexer.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE);
                break;
        }
    }

    #popByBRACE() {
        this.#paren_or_bracket_openedStack.pop();
        const curBraceExpression = this.#braceExpressionStack.pop();
        this.#prevBraceExpression = curBraceExpression + "}";
        if (this.#braceExpressionStack.length > 0) {
            // Extend the current brace expression by adding the previous expression
            const lastIndex = this.#braceExpressionStack.length - 1;
            this.#braceExpressionStack[lastIndex] += this.#prevBraceExpression;
        }
    }

    #handleFSTRING_MIDDLEtokenWithDoubleBrace() { // ISTRING = interpolated string (FSTRING or TSTRING)
        // replace the trailing double brace with a single brace and insert a hidden brace token
        const lastTwoChars = this.#getLastTwoCharsOfTheCurTokenText();
        switch (lastTwoChars) {
            case "{{":
                this.#trimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", Token.HIDDEN_CHANNEL);
                break;
            case "}}":
                this.#trimLastCharAddPendingTokenSetCurToken(PythonLexer.RBRACE, "}", Token.HIDDEN_CHANNEL);
                break;
        }
    }

    #handleFSTRING_MIDDLEtokenWithQuoteAndLBrace() { // ISTRING = interpolated string (FSTRING or TSTRING)
        // replace the trailing     quote + left_brace with a quote     and insert an LBRACE token
        // replace the trailing backslash + left_brace with a backslash and insert an LBRACE token        
        const lastTwoChars = this.#getLastTwoCharsOfTheCurTokenText();
        switch (lastTwoChars) {
            case "\"{":
            case "'{":
            case "\\{":
                this.#trimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", Token.DEFAULT_CHANNEL);
                break;
        }
    }

    #getLastTwoCharsOfTheCurTokenText() {
        const text = this.#curToken.text;
        return text.length <= 2 ? text : text.slice(-2);
    }

    #trimLastCharAddPendingTokenSetCurToken(type, text, channel) {
        // trim the last char and add the modified curToken to the pendingTokens stack
        const tokenTextWithoutLastChar = this.#curToken.text.slice(0, -1);
        this.#curToken.text = tokenTextWithoutLastChar;
        this.#curToken.stop -= 1;
        this.#addPendingToken(this.#curToken);

        this.#createNewCurToken(type, text, channel); // set curToken
    }

    #handleCOLONEQUALtokenInIString() { // ISTRING = interpolated string (FSTRING or TSTRING)
        if (this.#lexerModeStack.length > 0 &&
            this.#paren_or_bracket_openedStack.at(-1) === 0) { // stack peek === 0

            // In an f/t-string, the walrus operator (:=) is only allowed inside parentheses.
            // If used outside, split the COLONEQUAL token into a COLON
            // (used as a format specifier instead of a walrus operator),
            // and move the equal sign to the beginning of the next token (FSTRING_MIDDLE or TSTRING_MIDDLE).
            this.#curToken.type = PythonLexer.COLON;
            this.#curToken.text = ":";
            this.#curToken.stop = this.#curToken.start;

            switch (this.#ffgToken.type) {
                case PythonLexer.FSTRING_MIDDLE:
                case PythonLexer.TSTRING_MIDDLE: {
                    const token = this.#ffgToken.clone();
                    token.text = "=" + token.text;
                    token.start -= 1;
                    token.column -= 1;
                    this.#ffgToken = token;
                    break;
                }
                default: {
                    this.#addPendingToken(this.#curToken);
                    this.#createNewCurToken(this.#curISTRING_MIDDLEtokenType, "=", Token.DEFAULT_CHANNEL);
                }
            }
        }
        this.#addPendingToken(this.#curToken);
    }

    #createNewCurToken(type, text, channel) {
        const token = this.#curToken.clone();
        token.type = type;
        token.text = text;
        token.channel = channel;
        token.column += 1;
        token.start += 1;
        token.stop = token.start;
        this.#curToken = token;
    }

    #pushLexerMode(mode) {
        this.pushMode(mode);
        this.#lexerModeStack.push(this.#curLexerMode);
        this.#curLexerMode = mode;
    }

    #popLexerMode() {
        this.popMode();
        this.#curLexerMode = this.#lexerModeStack.pop();
    }

    #handleFORMAT_SPECIFICATION_MODE() {
        if (this.#lexerModeStack.length == 0 || this.#ffgToken.type !== PythonLexer.RBRACE) {
            return;
        }

        // insert an empty FSTRING_MIDDLE or TSTRING_MIDDLE token instead of the missing format specification
        switch (this.#curToken.type) {
            case PythonLexer.COLON:
                this.#createAndAddPendingToken(this.#curISTRING_MIDDLEtokenType, "", this.#ffgToken);
                break;
            case PythonLexer.RBRACE:
                // only when the previous brace expression is not a dictionary comprehension or set comprehension
                if (!this.#isValid_DictionaryOrSet_ComprehensionExpression(this.#prevBraceExpression)) {
                    this.#createAndAddPendingToken(this.#curISTRING_MIDDLEtokenType, "", this.#ffgToken);
                }
                break;
        }
    }

    #isValid_DictionaryOrSet_ComprehensionExpression(code) {
        const inputStream = CharStreams.fromString(code);
        const lexer = new PythonLexer(inputStream);
        const tokenStream = new CommonTokenStream(lexer);
        let parser = new PythonParser(tokenStream);

        // Disable error listeners to suppress console output
        lexer.removeErrorListeners();
        parser.removeErrorListeners();

        parser.dictcomp(); // Try parsing as dictionary comprehension
        if (parser.syntaxErrorsCount === 0)
            return true;

        parser = new PythonParser(tokenStream);
        tokenStream.seek(0);
        parser.removeErrorListeners();
        parser.setcomp(); // Try parsing as set comprehension
        return parser.syntaxErrorsCount === 0;
    }

    #insertTrailingTokens() {
        switch (this.#lastPendingTokenTypeFromDefaultChannel) {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default:
                // insert an extra trailing NEWLINE token that serves as the end of the last statement
                this.#createAndAddPendingToken(PythonLexer.NEWLINE, null, this.#ffgToken); // ffgToken is EOF
        }
        this.#insertIndentOrDedentToken(0); // Now insert as much trailing DEDENT tokens as needed
    }

    #handleEOFtoken() {
        if (this.#lastPendingTokenTypeFromDefaultChannel > 0) {
            // there was a statement in the input (leading NEWLINE tokens are hidden)
            this.#insertTrailingTokens();
        }
        this.#addPendingToken(this.#curToken);
    }

    #hideAndAddPendingToken(originalToken) {
        originalToken.channel = Token.HIDDEN_CHANNEL;
        this.#addPendingToken(originalToken);
    }

    #createAndAddPendingToken(type, text, originalToken) {
        const token = originalToken.clone();
        token.type = type;
        token.channel = Token.DEFAULT_CHANNEL;
        token.stop = originalToken.start - 1;
        token.text = text == null ?
            `<${PythonLexer.symbolicNames[type] ?? ""}>` :
            text;

        this.#addPendingToken(token);
    }

    #addPendingToken(token) {
        // save the last pending token type because the pendingTokens linked list can be empty by the nextToken()
        this.#previousPendingTokenType = token.type;
        if (token.channel === Token.DEFAULT_CHANNEL) {
            this.#lastPendingTokenTypeFromDefaultChannel = this.#previousPendingTokenType;
        }
        this.#pendingTokens.push(token) /* .addLast(token) */;
    }

    #getIndentationLength(indentText) { // the indentText may contain spaces, tabs or form feeds
        let length = 0;
        for (let ch of indentText) {
            switch (ch) {
                case " ":
                    this.#wasSpaceIndentation = true;
                    length += 1;
                    break;
                case "\t":
                    this.#wasTabIndentation = true;
                    length += PythonLexerBase.#TAB_LENGTH - (length % PythonLexerBase.#TAB_LENGTH);
                    break;
                case "\f": // form feed
                    length = 0;
                    break;
            }
        }

        if (this.#wasTabIndentation && this.#wasSpaceIndentation) {
            if (!this.#wasIndentationMixedWithSpacesAndTabs) {
                this.#wasIndentationMixedWithSpacesAndTabs = true;
                length = PythonLexerBase.#INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    #reportLexerError(errMsg) {
        this.getErrorListener().syntaxError(this, this.#curToken.type, this.#curToken.line, this.#curToken.column, " LEXER" + PythonLexerBase.#ERR_TXT + errMsg, null);
    }

    #reportError(errMsg) {
        this.#reportLexerError(errMsg);

        this.#createAndAddPendingToken(PythonLexer.ERRORTOKEN, PythonLexerBase.#ERR_TXT + errMsg, this.#ffgToken);
        // the ERRORTOKEN also triggers a parser error 
    }
}
