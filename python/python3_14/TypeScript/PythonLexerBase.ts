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

import { CharStream, CharStreams, CommonTokenStream, Token, CommonToken, Lexer } from "antlr4";
import { TokenSource } from "antlr4/src/antlr4/TokenSource.js";
import PythonLexer from "./PythonLexer.js";
import PythonParser from "./PythonParser.js";
import * as Collections from "typescript-collections";

export default abstract class PythonLexerBase extends Lexer {
    private static readonly LEXER_MODES_FOR_ISTRING_START: Map<string, number> = new Map<string, number>();
    private static readonly INVALID_LENGTH: number = -1;
    private static readonly ERR_TXT: string = " ERROR: ";
    private static readonly TAB_LENGTH: number = 8;

    private encodingName!: string;

    // Indentation handling
    private indentLengthStack!: Collections.Stack<number>;
    private pendingTokens!: Array<Token>;

    private previousPendingTokenType!: number;
    private lastPendingTokenTypeFromDefaultChannel!: number;

    // Parenthesis / bracket / brace counts
    private opened!: number;
    private paren_or_bracket_openedStack!: Array<number>;
    private braceExpressionStack!: Array<string>;
    private prevBraceExpression!: string;

    // Current interpolated STRING_MIDDLE token type (FSTRING_MIDDLE or TSTRING_MIDDLE)
    private curISTRING_MIDDLEtokenType!: number;

    // We reimplement mode/stack because not all runtimes expose _mode/_modeStack
    private curLexerMode!: number;
    private lexerModeStack!: Array<number>;

    // Indentation diagnostics
    private wasSpaceIndentation!: boolean;
    private wasTabIndentation!: boolean;
    private wasIndentationMixedWithSpacesAndTabs!: boolean;

    // Current / lookahead tokens
    private curToken: Token | undefined;
    private ffgToken: Token | undefined;

    protected constructor(input: CharStream) {
        super(input);
        this.init();
    }

    public reset(): void {
        this.init();
        super.reset();
    }

    private init(): void {
        this.encodingName = "";
        this.indentLengthStack = new Collections.Stack<number>();
        this.pendingTokens = [];
        this.previousPendingTokenType = 0;
        this.lastPendingTokenTypeFromDefaultChannel = 0;
        this.opened = 0;
        this.paren_or_bracket_openedStack = [];
        this.braceExpressionStack = [];
        this.prevBraceExpression = "";
        this.curISTRING_MIDDLEtokenType = 0;
        this.curLexerMode = Lexer.DEFAULT_MODE;
        this.lexerModeStack = [];
        this.wasSpaceIndentation = false;
        this.wasTabIndentation = false;
        this.wasIndentationMixedWithSpacesAndTabs = false;
        this.curToken = undefined;
        this.ffgToken = undefined;
    }

    /**
     * Sets the encoding name to emit an ENCODING token at the start of the token stream.
     * Leave empty if not needed (e.g., when parsing from string).
     *
     * @param encodingName - The encoding name (e.g., "utf-8"), or empty string to disable ENCODING token.
     */
    public setEncodingName(encodingName: string): void {
        this.encodingName = encodingName;
    }

    public nextToken(): Token { // Reading the input stream until EOF is reached
        this.checkNextToken();
        return this.pendingTokens.shift()!; /* .pollFirst() */; // Add the queued token to the token stream
    }

    private checkNextToken(): void {
        if (this.previousPendingTokenType == PythonLexer.EOF)
            return;

        this.setCurrentAndFollowingTokens();
        if (this.indentLengthStack.isEmpty()) { // We're at the first token
            this.handleStartOfInput();
        }

        switch (this.curToken!.type) {
            case PythonLexer.NEWLINE:
                this.handleNEWLINEtoken();
                break;
            case PythonLexer.LPAR:
            case PythonLexer.LSQB:
            case PythonLexer.LBRACE:
                this.opened++;
                this.addPendingToken(this.curToken!);
                break;
            case PythonLexer.RPAR:
            case PythonLexer.RSQB:
            case PythonLexer.RBRACE:
                this.opened--;
                this.addPendingToken(this.curToken!);
                break;
            case PythonLexer.FSTRING_MIDDLE:
            case PythonLexer.TSTRING_MIDDLE:
                this.handleISTRING_MIDDLEtokenWithDoubleBrace(); // does not affect the opened field
                this.addPendingToken(this.curToken!);
                break;
            case PythonLexer.COLONEQUAL:
                this.handleCOLONEQUALtokenInIString();
                break;
            case PythonLexer.ERRORTOKEN:
                this.reportLexerError(`token recognition error at: '${this.curToken!.text}'`);
                this.addPendingToken(this.curToken!);
                break;
            case PythonLexer.EOF:
                this.handleEOFtoken();
                break;
            default:
                this.addPendingToken(this.curToken!);
        }
        this.handleFORMAT_SPECIFICATION_MODE();
    }

    private setCurrentAndFollowingTokens(): void {
        this.curToken = this.ffgToken == undefined
            ? super.nextToken()
            : this.ffgToken;

        this.checkCurToken(); // Do not use ffgToken in this method or any of its submethods — it hasn't been set yet!

        this.ffgToken = this.curToken!.type === PythonLexer.EOF
            ? this.curToken
            : super.nextToken();
    }

    // - initialize indent stack
    // - skip BOM token
    // - insert ENCODING token (if any)
    // - hide leading NEWLINE(s)
    // - insert leading INDENT if first statement is indented
    private handleStartOfInput(): void {
        // initialize the stack with a default 0 indentation length
        this.indentLengthStack.push(0); // this will never be popped off

        if (this.curToken!.type === PythonLexer.BOM) {
            this.setCurrentAndFollowingTokens();
        }
        this.insertENCODINGtoken();

        while (this.curToken!.type !== PythonLexer.EOF) {
            if (this.curToken!.channel === Token.DEFAULT_CHANNEL) {
                if (this.curToken!.type === PythonLexer.NEWLINE) {
                    // all the NEWLINE tokens must be ignored before the first statement
                    this.hideAndAddPendingToken(this.curToken!);
                } else { // We're at the first statement
                    this.insertLeadingIndentToken();
                    return; // continue the processing of the current token with checkNextToken()
                }
            } else {
                this.addPendingToken(this.curToken!); // it can be WS, EXPLICIT_LINE_JOINING or COMMENT token
            }
            this.setCurrentAndFollowingTokens();
        } // continue the processing of the EOF token with checkNextToken()
    }

    private insertENCODINGtoken(): void { // https://peps.python.org/pep-0263/
        if (this.encodingName === '') return;

        const sourcePair = [this as unknown as TokenSource, this._input] as [TokenSource, CharStream];
        const encodingToken: CommonToken = new CommonToken(sourcePair, PythonLexer.ENCODING, Token.HIDDEN_CHANNEL, /*start*/ 0, /*stop*/ 0);
        encodingToken.text = this.encodingName;
        encodingToken.line = 0;
        encodingToken.column = -1;
        this.addPendingToken(encodingToken);
    }

    private insertLeadingIndentToken(): void {
        if (this.previousPendingTokenType === PythonLexer.WS) {
            const prevToken: Token = this.pendingTokens.at(-1)!; /* stack peek */ // WS token
            if (this.getIndentationLength(prevToken.text) !== 0) { // there is an "indentation" before the first statement
                const errMsg: string = "first statement indented";
                this.reportLexerError(errMsg);
                // insert an INDENT token before the first statement to trigger an 'unexpected indent' error later in the parser
                this.createAndAddPendingToken(PythonLexer.INDENT, PythonLexerBase.ERR_TXT + errMsg, this.curToken!);
            }
        }
    }

    private handleNEWLINEtoken(): void {
        if (this.lexerModeStack.length > 0) { // for multi line f/t-string literals
            this.addPendingToken(this.curToken!);
            return;
        }

        if (this.opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
            this.hideAndAddPendingToken(this.curToken!);
            return;
        }

        const nlToken: Token = this.curToken!.clone(); // save the current NEWLINE token
        const isLookingAhead: boolean = this.ffgToken!.type === PythonLexer.WS;
        if (isLookingAhead) {
            this.setCurrentAndFollowingTokens(); // set the next two tokens
        }

        switch (this.ffgToken!.type) {
            case PythonLexer.NEWLINE: // We're before a blank line
            case PythonLexer.COMMENT: // We're before a comment
                this.hideAndAddPendingToken(nlToken);
                if (isLookingAhead) {
                    this.addPendingToken(this.curToken!); // WS token
                }
                break;
            default:
                this.addPendingToken(nlToken);
                if (isLookingAhead) { // We're on whitespace(s) followed by a statement
                    const indentationLength: number = this.ffgToken!.type === PythonLexer.EOF ?
                        0 :
                        this.getIndentationLength(this.curToken!.text);

                    if (indentationLength !== PythonLexerBase.INVALID_LENGTH) {
                        this.addPendingToken(this.curToken!); // WS token
                        this.insertIndentOrDedentToken(indentationLength); // may insert INDENT token or DEDENT token(s)
                    } else {
                        this.reportError("inconsistent use of tabs and spaces in indentation");
                    }
                } else { // We're at a newline followed by a statement (there is no whitespace before the statement)
                    this.insertIndentOrDedentToken(0); // may insert DEDENT token(s)
                }
        }
    }

    private insertIndentOrDedentToken(indentLength: number): void {
        let prevIndentLength: number = this.indentLengthStack.peek()!;
        if (indentLength > prevIndentLength) {
            this.createAndAddPendingToken(PythonLexer.INDENT, null, this.ffgToken!);
            this.indentLengthStack.push(indentLength);
            return;
        }

        while (indentLength < prevIndentLength) { // more than 1 DEDENT token may be inserted to the token stream
            this.indentLengthStack.pop();
            prevIndentLength = this.indentLengthStack.peek()!;
            if (indentLength <= prevIndentLength) {
                this.createAndAddPendingToken(PythonLexer.DEDENT, null, this.ffgToken!);
            } else {
                this.reportError("inconsistent dedent");
            }
        }
    }

    private checkCurToken(): void {
        switch (this.curToken!.type) {
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
                switch (this.curToken!.type) {
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
                if (this.lexerModeStack.length === 0) {
                    return; // Not in f/t-string mode
                }
        }
        this.processBraceExpression();
    }

    private processBraceExpression(): void {
        switch (this.curToken!.type) { // the following tokens can only come from default mode (after an LBRACE in f/t-string)
            case PythonLexer.NEWLINE:
                // append the current brace expression with the current newline
                this.appendToBraceExpression(this.curToken!.text);
                this.curToken!.channel = Token.HIDDEN_CHANNEL;
                break;
            case PythonLexer.LBRACE:
                // the outermost brace expression cannot be a dictionary comprehension or a set comprehension
                this.braceExpressionStack.push("{");
                this.paren_or_bracket_openedStack.push(0);
                this.pushLexerMode(Lexer.DEFAULT_MODE);
                break;
            case PythonLexer.LPAR:
            case PythonLexer.LSQB:
                // append the current brace expression with a "(" or a "["
                this.appendToBraceExpression(this.curToken!.text);
                // https://peps.python.org/pep-0498/#lambdas-inside-expressions
                this.incrementBraceStack();
                break;
            case PythonLexer.RPAR:
            case PythonLexer.RSQB:
                // append the current brace expression with a ")" or a "]"
                this.appendToBraceExpression(this.curToken!.text);
                this.decrementBraceStack();
                break;
            case PythonLexer.COLON:
            case PythonLexer.COLONEQUAL:
                // append the current brace expression with a ":" or a ":="
                this.appendToBraceExpression(this.curToken!.text);
                this.setLexerModeByCOLONorCOLONEQUALtoken();
                break;
            case PythonLexer.RBRACE:
                this.setLexerModeAfterRBRACEtoken();
                break;
            default:
                // append the current brace expression with the current token text
                this.appendToBraceExpression(this.curToken!.text);
        }
    }

    private appendToBraceExpression(text: string): void {
        const lastIndex: number = this.braceExpressionStack.length - 1;
        this.braceExpressionStack[lastIndex] += text;
    }

    private incrementBraceStack(): void { // increment the last element (stack peek + 1)
        const lastIndex: number = this.paren_or_bracket_openedStack.length - 1;
        this.paren_or_bracket_openedStack[lastIndex]!++;
    }

    private decrementBraceStack(): void { // decrement the last element (stack peek - 1)
        const lastIndex: number = this.paren_or_bracket_openedStack.length - 1;
        this.paren_or_bracket_openedStack[lastIndex]!--;
    }

    private setLexerModeAfterRBRACEtoken(): void {
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

    private setLexerModeByISTRING_STARTtoken(): void { // ISTRING = interpolated string (FSTRING or TSTRING)
        if (PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.size === 0) {
            PythonLexerBase.initLexerModesForIStringStart();
        }

        const interpolatedStringPrefix: string = this.curToken!.text.toLowerCase();
        if (PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.has(interpolatedStringPrefix)) {
            const newLexerMode: number = PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.get(interpolatedStringPrefix)!;
            this.pushLexerMode(newLexerMode);
        } else {
            this.reportLexerError(
                "internal error: unknown interpolated string literal prefix: " + this.curToken!.text
            );
        }
    }

    private static initLexerModesForIStringStart(): void {
        // f-strings
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("f'", PythonLexer.SQ1__FSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("rf'", PythonLexer.SQ1R_FSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("fr'", PythonLexer.SQ1R_FSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("f\"", PythonLexer.DQ1__FSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("rf\"", PythonLexer.DQ1R_FSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("fr\"", PythonLexer.DQ1R_FSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("f'''", PythonLexer.SQ3__FSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("rf'''", PythonLexer.SQ3R_FSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("fr'''", PythonLexer.SQ3R_FSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("f\"\"\"", PythonLexer.DQ3__FSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("rf\"\"\"", PythonLexer.DQ3R_FSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("fr\"\"\"", PythonLexer.DQ3R_FSTRING_MODE);

        // t-strings
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("t'", PythonLexer.SQ1__TSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("rt'", PythonLexer.SQ1R_TSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("tr'", PythonLexer.SQ1R_TSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("t\"", PythonLexer.DQ1__TSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("rt\"", PythonLexer.DQ1R_TSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("tr\"", PythonLexer.DQ1R_TSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("t'''", PythonLexer.SQ3__TSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("rt'''", PythonLexer.SQ3R_TSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("tr'''", PythonLexer.SQ3R_TSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("t\"\"\"", PythonLexer.DQ3__TSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("rt\"\"\"", PythonLexer.DQ3R_TSTRING_MODE);
        PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.set("tr\"\"\"", PythonLexer.DQ3R_TSTRING_MODE);
    }

    private setLexerModeByCOLONorCOLONEQUALtoken(): void {
        // Exit early when the current lexer mode indicates an open parenthesis/bracket
        const opened: boolean = this.paren_or_bracket_openedStack.at(-1)! > 0; /* stack peek */
        if (opened) {
            return;
        }

        // COLONEQUAL token will be replaced with a COLON token in CheckNextToken()
        const prevLexerMode: number = this.lexerModeStack.at(-1)!; /* stack peek */
        switch (prevLexerMode) {
            case PythonLexer.SQ1__FSTRING_MODE:
            case PythonLexer.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ1__TSTRING_MODE:
            case PythonLexer.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ1R_FSTRING_MODE:
            case PythonLexer.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ1R_TSTRING_MODE:
            case PythonLexer.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ1__FSTRING_MODE:
            case PythonLexer.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ1__TSTRING_MODE:
            case PythonLexer.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ1R_FSTRING_MODE:
            case PythonLexer.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ1R_TSTRING_MODE:
            case PythonLexer.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ3__FSTRING_MODE:
            case PythonLexer.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ3__TSTRING_MODE:
            case PythonLexer.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ3R_FSTRING_MODE:
            case PythonLexer.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.SQ3R_TSTRING_MODE:
            case PythonLexer.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ3__FSTRING_MODE:
            case PythonLexer.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ3__TSTRING_MODE:
            case PythonLexer.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ3R_FSTRING_MODE:
            case PythonLexer.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE);
                break;

            case PythonLexer.DQ3R_TSTRING_MODE:
            case PythonLexer.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.pushLexerMode(PythonLexer.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE);
                break;
        }
    }

    private popByBRACE(): void {
        this.paren_or_bracket_openedStack.pop();
        const curBraceExpression: string = this.braceExpressionStack.pop()!;
        this.prevBraceExpression = curBraceExpression + "}";
        if (this.braceExpressionStack.length > 0) {
            // Extend the current brace expression by adding the previous expression
            const lastIndex: number = this.braceExpressionStack.length - 1;
            this.braceExpressionStack[lastIndex] += this.prevBraceExpression;
        }
    }

    private handleISTRING_MIDDLEtokenWithDoubleBrace(): void { // ISTRING = interpolated string (FSTRING or TSTRING)
        // Replace the trailing double brace with a single brace and insert a hidden brace token
        const lastTwoChars: string = this.getLastTwoCharsOfTheCurTokenText();
        switch (lastTwoChars) {
            case "{{":
                this.trimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", Token.HIDDEN_CHANNEL);
                break;
            case "}}":
                this.trimLastCharAddPendingTokenSetCurToken(PythonLexer.RBRACE, "}", Token.HIDDEN_CHANNEL);
                break;
        }
    }

    private handleISTRING_MIDDLEtokenWithQuoteAndLBrace(): void { // ISTRING = interpolated string (FSTRING or TSTRING)
        // Replace the trailing quote + left_brace with a quote and insert an LBRACE token
        // Replace the trailing backslash + left_brace with a backslash and insert an LBRACE token
        const lastTwoChars: string = this.getLastTwoCharsOfTheCurTokenText();
        switch (lastTwoChars) {
            case "\"{":
            case "'{":
            case "\\{":
                this.trimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", Token.DEFAULT_CHANNEL);
                break;
        }
    }

    private getLastTwoCharsOfTheCurTokenText(): string {
        const text = this.curToken!.text;
        return text.length <= 2 ? text : text.slice(-2);
    }

    private trimLastCharAddPendingTokenSetCurToken(type: number, text: string, channel: number): void {
        // Trim the last char and add the modified curToken to the pendingTokens stack
        const tokenTextWithoutLastChar: string = this.curToken!.text.slice(0, -1);
        this.curToken!.text = tokenTextWithoutLastChar;
        this.curToken!.stop -= 1;
        this.addPendingToken(this.curToken!);

        this.createNewCurToken(type, text, channel); // Set curToken
    }

    private handleCOLONEQUALtokenInIString(): void { // ISTRING = interpolated string (FSTRING or TSTRING)
        if (this.lexerModeStack.length > 0 &&
            this.paren_or_bracket_openedStack.at(-1) === 0) { // stack peek === 0

            // In an f-string, the walrus operator (:=) is only allowed inside parentheses.
            // If used outside, split the COLONEQUAL token into a COLON
            // (used as a format specifier instead of a walrus operator),
            // and move the equal sign to the beginning of the next token (FSTRING_MIDDLE or TSTRING_MIDDLE).
            this.curToken!.type = PythonLexer.COLON;
            this.curToken!.text = ":";
            this.curToken!.stop = this.curToken!.start;

            switch (this.ffgToken!.type) {
                case PythonLexer.FSTRING_MIDDLE:
                case PythonLexer.TSTRING_MIDDLE: {
                    const token: Token = this.ffgToken!.clone();
                    token.text = "=" + token.text;
                    token.start -= 1;
                    token.column -= 1;
                    this.ffgToken = token;
                    break;
                }
                default: {
                    this.addPendingToken(this.curToken!);
                    this.createNewCurToken(this.curISTRING_MIDDLEtokenType, "=", Token.DEFAULT_CHANNEL);
                }
            }

        }
        this.addPendingToken(this.curToken!);
    }

    private createNewCurToken(type: number, text: string, channel: number): void {
        const token: CommonToken = this.curToken!.clone();
        token.type = type;
        token.text = text;
        token.channel = channel;
        token.column += 1;
        token.start += 1;
        token.stop = token.start;
        this.curToken = token;
    }

    private pushLexerMode(mode: number): void {
        this.pushMode(mode);
        this.lexerModeStack.push(this.curLexerMode);
        this.curLexerMode = mode;
    }

    private popLexerMode(): void {
        this.popMode();
        this.curLexerMode = this.lexerModeStack.pop()!;
    }

    private handleFORMAT_SPECIFICATION_MODE(): void {
        if (this.lexerModeStack.length == 0 || this.ffgToken!.type !== PythonLexer.RBRACE) {
            return;
        }

        // insert an empty FSTRING_MIDDLE or TSTRING_MIDDLE token instead of the missing format specification
        switch (this.curToken!.type) {
            case PythonLexer.COLON:
                this.createAndAddPendingToken(this.curISTRING_MIDDLEtokenType, "", this.ffgToken!);
                break;
            case PythonLexer.RBRACE:
                // only when the previous brace expression is not a dictionary comprehension or set comprehension
                if (!this.isValid_DictionaryOrSet_ComprehensionExpression(this.prevBraceExpression)) {
                    this.createAndAddPendingToken(this.curISTRING_MIDDLEtokenType, "", this.ffgToken!);
                }
                break;
        }
    }

    private isValid_DictionaryOrSet_ComprehensionExpression(code: string): boolean {
        const inputStream: CharStream = CharStreams.fromString(code);
        const lexer: PythonLexer = new PythonLexer(inputStream);
        const tokenStream: CommonTokenStream = new CommonTokenStream(lexer);
        let parser = new PythonParser(tokenStream);

        // Disable error listeners to suppress console output
        lexer.removeErrorListeners();
        parser.removeErrorListeners();

        parser.dictcomp(); // Try parsing as dictionary comprehension
        if (parser.syntaxErrorsCount === 0) {
            return true;
        }

        parser = new PythonParser(tokenStream);
        (tokenStream as any).seek(0); // seek method is not declared in CommonTokenStream.d.ts
        parser.removeErrorListeners();
        parser.setcomp(); // Try parsing as set comprehension
        return parser.syntaxErrorsCount === 0;
    }

    private insertTrailingTokens(): void {
        switch (this.lastPendingTokenTypeFromDefaultChannel) {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default:
                // insert an extra trailing NEWLINE token that serves as the end of the last statement
                this.createAndAddPendingToken(PythonLexer.NEWLINE, null, this.ffgToken!); // ffgToken is EOF
        }
        this.insertIndentOrDedentToken(0); // Now insert as much trailing DEDENT tokens as needed
    }

    private handleEOFtoken(): void {
        if (this.lastPendingTokenTypeFromDefaultChannel > 0) {
            // there was a statement in the input (leading NEWLINE tokens are hidden)
            this.insertTrailingTokens();
        }
        this.addPendingToken(this.curToken!);
    }

    private hideAndAddPendingToken(originalToken: Token): void {
        originalToken.channel = Token.HIDDEN_CHANNEL;
        this.addPendingToken(originalToken);
    }

    private createAndAddPendingToken(type: number, text: string | null, originalToken: Token): void {
        const token: Token = originalToken.clone();
        token.type = type;
        token.channel = Token.DEFAULT_CHANNEL;
        token.stop = originalToken.start - 1;
        token.text = text == null ?
            `<${PythonLexer.symbolicNames[type] ?? ""}>` :
            text;

        this.addPendingToken(token);
    }

    private addPendingToken(token: Token): void {
        // save the last pending token type because the pendingTokens list can be empty by the nextToken()
        this.previousPendingTokenType = token.type;
        if (token.channel === Token.DEFAULT_CHANNEL) {
            this.lastPendingTokenTypeFromDefaultChannel = this.previousPendingTokenType;
        }
        this.pendingTokens.push(token) /* .addLast(token) */;
    }

    private getIndentationLength(indentText: string): number { // the indentText may contain spaces, tabs or form feeds
        let length: number = 0;
        for (let ch of indentText) {
            switch (ch) {
                case " ":
                    this.wasSpaceIndentation = true;
                    length += 1;
                    break;
                case "\t":
                    this.wasTabIndentation = true;
                    length += PythonLexerBase.TAB_LENGTH - (length % PythonLexerBase.TAB_LENGTH);
                    break;
                case "\f": // form feed
                    length = 0;
                    break;
            }
        }

        if (this.wasTabIndentation && this.wasSpaceIndentation) {
            if (!this.wasIndentationMixedWithSpacesAndTabs) {
                this.wasIndentationMixedWithSpacesAndTabs = true;
                length = PythonLexerBase.INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    private reportLexerError(errMsg: string): void {
        this.getErrorListener().syntaxError(this, this.curToken!.type, this.curToken!.line, this.curToken!.column, " LEXER" + PythonLexerBase.ERR_TXT + errMsg, undefined);
    }

    private reportError(errMsg: string): void {
        this.reportLexerError(errMsg);

        this.createAndAddPendingToken(PythonLexer.ERRORTOKEN, PythonLexerBase.ERR_TXT + errMsg, this.ffgToken!);
        // the ERRORTOKEN also triggers a parser error 
    }
}