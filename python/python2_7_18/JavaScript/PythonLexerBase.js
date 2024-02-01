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

import { Token, CommonToken, Lexer } from "antlr4";
import PythonLexer from "./PythonLexer.js";

export default class PythonLexerBase extends Lexer {
    constructor(input) {
        super(input);

        // A stack that keeps track of the indentation lengths
        this.indentLengthStack;
        // A list where tokens are waiting to be loaded into the token stream
        this.pendingTokens;

        // last pending token types
        this.previousPendingTokenType;
        this.lastPendingTokenTypeFromDefaultChannel;

        // The amount of opened parentheses, square brackets or curly braces
        this.opened;

        this.wasSpaceIndentation;
        this.wasTabIndentation;
        this.wasIndentationMixedWithSpacesAndTabs;
        const INVALID_LENGTH = -1;
        
        this.curToken; // current (under processing) token
        this.ffgToken; // following (look ahead) token

        const ERR_TXT = " ERROR: ";

        this.init();
    }

    init() {
        this.indentLengthStack = [];
        this.pendingTokens = [];
        this.previousPendingTokenType = 0;
        this.lastPendingTokenTypeFromDefaultChannel = 0;
        this.opened = 0;
        this.wasSpaceIndentation = false;
        this.wasTabIndentation = false;
        this.wasIndentationMixedWithSpacesAndTabs = false;
        this.curToken = null;
        this.ffgToken = null;
    }

    nextToken() { // reading the input stream until a return EOF
        this.checkNextToken();
        return this.pendingTokens.shift() /* .pollFirst() */; // add the queued token to the token stream
    }

    checkNextToken() {
        if (this.previousPendingTokenType !== Token.EOF) {
            this.setCurrentAndFollowingTokens();
            if (this.indentLengthStack.length === 0) { // We're at the first token
                this.handleStartOfInput();
            }

            switch (this.curToken.type) {
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
                case PythonLexer.NEWLINE:
                    this.handleNEWLINEtoken();
                    break;
                case PythonLexer.STRING:
                    this.handleSTRINGtoken();
                    break;
                case PythonLexer.ERROR_TOKEN:
                    this.reportLexerError(`token recognition error at: '${this.curToken.text}'`);
                    this.addPendingToken(this.curToken);
                    break;
                case Token.EOF:
                    this.handleEOFtoken();
                    break;
                default:
                    this.addPendingToken(this.curToken);
            }
        }
    }

    setCurrentAndFollowingTokens() {
        this.curToken = this.ffgToken == undefined ?
            this.getCommonTokenByToken(super.nextToken()) :
            this.getCommonTokenByToken(this.ffgToken);

        this.ffgToken = this.curToken.type === Token.EOF ?
            this.curToken :
            this.getCommonTokenByToken(super.nextToken());
    }

    // initialize the _indentLengthStack
    // hide the leading NEWLINE token(s)
    // if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    // insert a leading INDENT token if necessary
    handleStartOfInput() {
        // initialize the stack with a default 0 indentation length
        this.indentLengthStack.push(0); // this will never be popped off
        while (this.curToken.type !== Token.EOF) {
            if (this.curToken.channel === Token.DEFAULT_CHANNEL) {
                if (this.curToken.type === PythonLexer.NEWLINE) {
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
        } // continue the processing of the EOF token with checkNextToken()
    }

    insertLeadingIndentToken() {
        if (this.previousPendingTokenType === PythonLexer.WS) {
            let prevToken = this.pendingTokens.at(- 1) /* .peekLast() */; // WS token
            if (this.getIndentationLength(prevToken.text) !== 0) { // there is an "indentation" before the first statement
                const errMsg = "first statement indented";
                this.reportLexerError(errMsg);
                // insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
                this.createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, this.ERR_TXT + errMsg, this.curToken);
            }
        }
    }

    handleNEWLINEtoken() {
        if (this.opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
            this.hideAndAddPendingToken(this.curToken);
        } else {
            let nlToken = this.getCommonTokenByToken(this.curToken); // save the current NEWLINE token
            const isLookingAhead = this.ffgToken.type === PythonLexer.WS;
            if (isLookingAhead) {
                this.setCurrentAndFollowingTokens(); // set the next two tokens
            }

            switch (this.ffgToken.type) {
                case PythonLexer.NEWLINE:      // We're before a blank line
                case PythonLexer.COMMENT:      // We're before a comment
                    this.hideAndAddPendingToken(nlToken);
                    if (isLookingAhead) {
                        this.addPendingToken(this.curToken);  // WS token
                    }
                    break;
                default:
                    this.addPendingToken(nlToken);
                    if (isLookingAhead) { // We're on whitespace(s) followed by a statement
                        const indentationLength = this.ffgToken.type === Token.EOF ?
                                                  0 :
                                                  this.getIndentationLength(this.curToken.text);

                        if (indentationLength !== this.INVALID_LENGTH) {
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
    }

    insertIndentOrDedentToken(curIndentLength) {
        let prevIndentLength = this.indentLengthStack.at(-1) /* peek() */;
        if (curIndentLength > prevIndentLength) {
            this.createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, null, this.ffgToken);
            this.indentLengthStack.push(curIndentLength);
        } else {
            while (curIndentLength < prevIndentLength) { // more than 1 DEDENT token may be inserted to the token stream
                this.indentLengthStack.pop();
                prevIndentLength = this.indentLengthStack.at(-1) /* peek() */;
                if (curIndentLength <= prevIndentLength) {
                    this.createAndAddPendingToken(PythonLexer.DEDENT, Token.DEFAULT_CHANNEL, null, this.ffgToken);
                } else {
                    this.reportError("inconsistent dedent");
                }
            }
        }
    }

    handleSTRINGtoken() { // remove the \<newline> escape sequences from the string literal
        const line_joinFreeStringLiteral = this.curToken.text.replace(/\\(\r?\n)/g, "");
        if (this.curToken.text.length === line_joinFreeStringLiteral.length) {
            this.addPendingToken(this.curToken);
        } else {
            let originalSTRINGtoken = this.getCommonTokenByToken(this.curToken); // backup the original token
            this.curToken.text = line_joinFreeStringLiteral;
            this.addPendingToken(this.curToken); // add the modified token with inline string literal
            this.hideAndAddPendingToken(originalSTRINGtoken); // add the original token to the hidden channel
            // this inserted hidden token allows to restore the original string literal with the \<newline> escape sequences
        }
    }

    insertTrailingTokens() {
        switch (this.lastPendingTokenTypeFromDefaultChannel) {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default:
                // insert an extra trailing NEWLINE token that serves as the end of the last statement
                this.createAndAddPendingToken(PythonLexer.NEWLINE, Token.DEFAULT_CHANNEL, null, this.ffgToken); // _ffgToken is EOF
        }
        this.insertIndentOrDedentToken(0); // Now insert as much trailing DEDENT tokens as needed
    }

    handleEOFtoken() {
        if (this.lastPendingTokenTypeFromDefaultChannel > 0) {
            // there was a statement in the input (leading NEWLINE tokens are hidden)
            this.insertTrailingTokens();
        }
        this.addPendingToken(this.curToken);
    }

    hideAndAddPendingToken(cToken) {
        cToken.channel = Token.HIDDEN_CHANNEL;
        this.addPendingToken(cToken);
    }

    createAndAddPendingToken(type, channel, text, baseToken) {
        const cToken = this.getCommonTokenByToken(baseToken);
        cToken.type = type;
        cToken.channel = channel;
        cToken.stop = baseToken.start - 1;
        cToken.text = text == null ?
            `<${this.getSymbolicNames()[type]}>` :
            text;

        this.addPendingToken(cToken);
    }

    addPendingToken(token) {
        // save the last pending token type because the _pendingTokens linked list can be empty by the nextToken()
        this.previousPendingTokenType = token.type;
        if (token.channel === Token.DEFAULT_CHANNEL) {
            this.lastPendingTokenTypeFromDefaultChannel = this.previousPendingTokenType;
        }
        this.pendingTokens.push(token) /* .addLast(token) */;
    }

    getCommonTokenByToken(oldToken) {
        let commonToken = new CommonToken(oldToken.source, oldToken.type, oldToken.channel, oldToken.start, oldToken.stop);
        commonToken.tokenIndex = oldToken.tokenIndex;
        commonToken.line = oldToken.line;
        commonToken.column = oldToken.column;
        commonToken.text = oldToken.text;
        return commonToken;
    }

    getIndentationLength(textWS) { // the textWS may contain spaces, tabs or form feeds
        const TAB_LENGTH = 8; // the standard number of spaces to replace a tab to spaces
        let length = 0;

        for (let ch of textWS) {
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
                return this.INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    reportLexerError(errMsg) {
        this.getErrorListenerDispatch().syntaxError(this, this.curToken, this.curToken.line, this.curToken.column, " LEXER" + this.ERR_TXT + errMsg, null);
    }

    reportError(errMsg) {
        this.reportLexerError(errMsg);

        // the ERROR_TOKEN will raise an error in the parser
        this.createAndAddPendingToken(PythonLexer.ERROR_TOKEN, Token.DEFAULT_CHANNEL, this.ERR_TXT + errMsg, this.ffgToken);
    }

    reset() {
        this.init();
        super.reset();
    }
}
