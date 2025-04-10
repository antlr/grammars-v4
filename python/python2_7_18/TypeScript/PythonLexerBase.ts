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
 * Project      : Python Indent/Dedent handler for ANTLR4 grammars
 *
 * Developed by : Robert Einhorn, robert.einhorn.hu@gmail.com
 *
 */

import { CharStream, Token, Lexer } from "antlr4";
import PythonLexer from "./PythonLexer";
import * as Collections from "typescript-collections";

export default abstract class PythonLexerBase extends Lexer {
    // A stack that keeps track of the indentation lengths
    private indentLengthStack!: Collections.Stack<number>;
    // A list where tokens are waiting to be loaded into the token stream
    private pendingTokens!: Array<Token>;

    // last pending token types
    private previousPendingTokenType!: number;
    private lastPendingTokenTypeFromDefaultChannel!: number;

    // The amount of opened parentheses, square brackets or curly braces
    private opened!: number;

    private wasSpaceIndentation!: boolean;
    private wasTabIndentation!: boolean;
    private wasIndentationMixedWithSpacesAndTabs!: boolean;
    
    private curToken: Token | undefined; // current (under processing) token
    private ffgToken: Token | undefined; // following (look ahead) token

    private readonly INVALID_LENGTH: number = -1;
    private readonly ERR_TXT: string = " ERROR: ";

    protected constructor(input: CharStream) {
        super(input);
        this.init();
    }

    public nextToken(): Token { // reading the input stream until a return EOF
        this.checkNextToken();
        return this.pendingTokens.shift()! /* .pollFirst() */; // add the queued token to the token stream
    }

    public reset(): void {
        this.init();
        super.reset();
    }

    private init(): void {
        this.indentLengthStack = new Collections.Stack<number>();
        this.pendingTokens = [];
        this.previousPendingTokenType = 0;
        this.lastPendingTokenTypeFromDefaultChannel = 0;
        this.opened = 0;
        this.wasSpaceIndentation = false;
        this.wasTabIndentation = false;
        this.wasIndentationMixedWithSpacesAndTabs = false;
        this.curToken = undefined;
        this.ffgToken = undefined;
    }

    private checkNextToken(): void {
        if (this.previousPendingTokenType !== PythonLexer.EOF) {
            this.setCurrentAndFollowingTokens();
            if (this.indentLengthStack.isEmpty()) { // We're at the first token
                this.handleStartOfInput();
            }

            switch (this.curToken!.type) {
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
                case PythonLexer.NEWLINE:
                    this.handleNEWLINEtoken();
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
        }
    }

    private setCurrentAndFollowingTokens(): void {
        this.curToken = this.ffgToken == undefined
            ? super.nextToken()
            : this.ffgToken;

        this.ffgToken = this.curToken.type === PythonLexer.EOF
            ? this.curToken
            : super.nextToken();
    }

    // initialize the indentLengthStack
    // hide the leading NEWLINE token(s)
    // if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    // insert a leading INDENT token if necessary
    private handleStartOfInput(): void {
        // initialize the stack with a default 0 indentation length
        this.indentLengthStack.push(0); // this will never be popped off
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

    private insertLeadingIndentToken(): void {
        if (this.previousPendingTokenType === PythonLexer.WS) {
            const prevToken: Token = this.pendingTokens[this.pendingTokens.length - 1] /* .peekLast() */; // WS token
            if (this.getIndentationLength(prevToken.text) !== 0) { // there is an "indentation" before the first statement
                const errMsg: string = "first statement indented";
                this.reportLexerError(errMsg);
                // insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
                this.createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, this.ERR_TXT + errMsg, this.curToken!);
            }
        }
    }

    private handleNEWLINEtoken(): void {
        if (this.opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
            this.hideAndAddPendingToken(this.curToken!);
        } else {
            const nlToken: Token = this.curToken?.clone()!; // save the current NEWLINE token
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

                        if (indentationLength !== this.INVALID_LENGTH) {
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
    }

    private insertIndentOrDedentToken(indentLength: number): void {
        let prevIndentLength: number = this.indentLengthStack.peek()!;
        if (indentLength > prevIndentLength) {
            this.createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, null, this.ffgToken!);
            this.indentLengthStack.push(indentLength);
        } else {
            while (indentLength < prevIndentLength) { // more than 1 DEDENT token may be inserted to the token stream
                this.indentLengthStack.pop();
                prevIndentLength = this.indentLengthStack.peek()!;
                if (indentLength <= prevIndentLength) {
                    this.createAndAddPendingToken(PythonLexer.DEDENT, Token.DEFAULT_CHANNEL, null, this.ffgToken!);
                } else {
                    this.reportError("inconsistent dedent");
                }
            }
        }
    }

    private insertTrailingTokens(): void {
        switch (this.lastPendingTokenTypeFromDefaultChannel) {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default:
                // insert an extra trailing NEWLINE token that serves as the end of the last statement
                this.createAndAddPendingToken(PythonLexer.NEWLINE, Token.DEFAULT_CHANNEL, null, this.ffgToken!); // ffgToken is EOF
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

    private hideAndAddPendingToken(tkn: Token): void {
        tkn.channel = Token.HIDDEN_CHANNEL;
        this.addPendingToken(tkn);
    }

    private createAndAddPendingToken(type: number, channel: number, text: string | null, sampleToken: Token): void {
        const tkn: Token = sampleToken.clone();
        tkn.type = type;
        tkn.channel = channel;
        tkn.stop = sampleToken.start - 1;
        tkn.text = text == null ?
            `<${this.getSymbolicNames()[type]}>` :
            text;

        this.addPendingToken(tkn);
    }

    private addPendingToken(tkn: Token): void {
        // save the last pending token type because the pendingTokens linked list can be empty by the nextToken()
        this.previousPendingTokenType = tkn.type;
        if (tkn.channel === Token.DEFAULT_CHANNEL) {
            this.lastPendingTokenTypeFromDefaultChannel = this.previousPendingTokenType;
        }
        this.pendingTokens.push(tkn) /* .addLast(token) */;
    }

    private getIndentationLength(indentText: string): number { // the indentText may contain spaces, tabs or form feeds
        const TAB_LENGTH: number = 8; // the standard number of spaces to replace a tab to spaces
        let length: number = 0;
        for (let ch of indentText) {
            switch (ch) {
                case " ":
                    this.wasSpaceIndentation = true;
                    length += 1;
                    break;
                case "\t":
                    this.wasTabIndentation = true;
                    length += TAB_LENGTH - (length % TAB_LENGTH);
                    break;
                case "\f": // form feed
                    length = 0;
                    break;
            }
        }

        if (this.wasTabIndentation && this.wasSpaceIndentation) {
            if (!this.wasIndentationMixedWithSpacesAndTabs) {
                this.wasIndentationMixedWithSpacesAndTabs = true;
                length = this.INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    private reportLexerError(errMsg: string): void {
        this.getErrorListener().syntaxError(this, 0 /* this.curToken */, this.curToken!.line, this.curToken!.column, " LEXER" + this.ERR_TXT + errMsg, undefined);
    }

    private reportError(errMsg: string): void {
        this.reportLexerError(errMsg);

        // the ERRORTOKEN will raise an error in the parser
        this.createAndAddPendingToken(PythonLexer.ERRORTOKEN, Token.DEFAULT_CHANNEL, this.ERR_TXT + errMsg, this.ffgToken!);
    }
}
