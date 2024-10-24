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

import { Token, Lexer } from "antlr4";
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
        //  The amount of opened parentheses and square brackets in the current lexer mode
        this.paren_or_bracket_openedStack;

        this.wasSpaceIndentation;
        this.wasTabIndentation;
        this.wasIndentationMixedWithSpacesAndTabs;

        this.curToken; // current (under processing) token
        this.ffgToken; // following (look ahead) token

        this.#init();
    }

    get #INVALID_LENGTH() { return -1; }
    get #ERR_TXT() { return " ERROR: "; }

    nextToken() { // reading the input stream until a return EOF
        this.#checkNextToken();
        return this.pendingTokens.shift() /* .pollFirst() */; // add the queued token to the token stream
    }

    reset() {
        this.#init();
        super.reset();
    }

    #init() {
        this.indentLengthStack = [];
        this.pendingTokens = [];
        this.previousPendingTokenType = 0;
        this.lastPendingTokenTypeFromDefaultChannel = 0;
        this.opened = 0;
        this.paren_or_bracket_openedStack = [];
        this.wasSpaceIndentation = false;
        this.wasTabIndentation = false;
        this.wasIndentationMixedWithSpacesAndTabs = false;
        this.curToken = null;
        this.ffgToken = null;
    }

    #checkNextToken() {
        if (this.previousPendingTokenType !== Token.EOF) {
            this.#setCurrentAndFollowingTokens();
            if (this.indentLengthStack.length === 0) { // We're at the first token
                this.#handleStartOfInput();
            }

            switch (this.curToken.type) {
                case PythonLexer.LPAR:
                case PythonLexer.LSQB:
                case PythonLexer.LBRACE:
                    this.opened++;
                    this.#addPendingToken(this.curToken);
                    break;
                case PythonLexer.RPAR:
                case PythonLexer.RSQB:
                case PythonLexer.RBRACE:
                    this.opened--;
                    this.#addPendingToken(this.curToken);
                    break;
                case PythonLexer.NEWLINE:
                    this.#handleNEWLINEtoken();
                    break;
                case PythonLexer.FSTRING_MIDDLE:
                    this.#handleFSTRING_MIDDLE_token();
                    break;
                case PythonLexer.ERRORTOKEN:
                    this.#reportLexerError(`token recognition error at: '${this.curToken.text}'`);
                    this.#addPendingToken(this.curToken);
                    break;
                case Token.EOF:
                    this.#handleEOFtoken();
                    break;
                default:
                    this.#addPendingToken(this.curToken);
            }
            this.#handleFORMAT_SPECIFICATION_MODE();
        }
    }

    #setCurrentAndFollowingTokens() {
        this.curToken = this.ffgToken == undefined ?
            super.nextToken() :
            this.ffgToken;

        this.#handleFStringLexerModes();

        this.ffgToken = this.curToken.type === Token.EOF ?
            this.curToken :
            super.nextToken();
    }

    // initialize the _indentLengthStack
    // hide the leading NEWLINE token(s)
    // if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    // insert a leading INDENT token if necessary
    #handleStartOfInput() {
        // initialize the stack with a default 0 indentation length
        this.indentLengthStack.push(0); // this will never be popped off
        while (this.curToken.type !== Token.EOF) {
            if (this.curToken.channel === Token.DEFAULT_CHANNEL) {
                if (this.curToken.type === PythonLexer.NEWLINE) {
                    // all the NEWLINE tokens must be ignored before the first statement
                    this.#hideAndAddPendingToken(this.curToken);
                } else { // We're at the first statement
                    this.#insertLeadingIndentToken();
                    return; // continue the processing of the current token with #checkNextToken()
                }
            } else {
                this.#addPendingToken(this.curToken); // it can be WS, EXPLICIT_LINE_JOINING or COMMENT token
            }
            this.#setCurrentAndFollowingTokens();
        } // continue the processing of the EOF token with #checkNextToken()
    }

    #insertLeadingIndentToken() {
        if (this.previousPendingTokenType === PythonLexer.WS) {
            let prevToken = this.pendingTokens.at(- 1) /* .peekLast() */; // WS token
            if (this.#getIndentationLength(prevToken.text) !== 0) { // there is an "indentation" before the first statement
                const errMsg = "first statement indented";
                this.#reportLexerError(errMsg);
                // insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
                this.#createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, this.#ERR_TXT + errMsg, this.curToken);
            }
        }
    }

    #handleNEWLINEtoken() {
        if (this.opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
            this.#hideAndAddPendingToken(this.curToken);
        } else {
            let nlToken = this.curToken.clone(); // save the current NEWLINE token
            const isLookingAhead = this.ffgToken.type === PythonLexer.WS;
            if (isLookingAhead) {
                this.#setCurrentAndFollowingTokens(); // set the next two tokens
            }

            switch (this.ffgToken.type) {
                case PythonLexer.NEWLINE: // We're before a blank line
                case PythonLexer.COMMENT: // We're before a comment
                    this.#hideAndAddPendingToken(nlToken);
                    if (isLookingAhead) {
                        this.#addPendingToken(this.curToken); // WS token
                    }
                    break;
                default:
                    this.#addPendingToken(nlToken);
                    if (isLookingAhead) { // We're on whitespace(s) followed by a statement
                        const indentationLength = this.ffgToken.type === Token.EOF ?
                            0 :
                            this.#getIndentationLength(this.curToken.text);

                        if (indentationLength !== this.#INVALID_LENGTH) {
                            this.#addPendingToken(this.curToken); // WS token
                            this.#insertIndentOrDedentToken(indentationLength); // may insert INDENT token or DEDENT token(s)
                        } else {
                            this.#reportError("inconsistent use of tabs and spaces in indentation");
                        }
                    } else { // We're at a newline followed by a statement (there is no whitespace before the statement)
                        this.#insertIndentOrDedentToken(0); // may insert DEDENT token(s)
                    }
            }
        }
    }

    #insertIndentOrDedentToken(curIndentLength) {
        let prevIndentLength = this.indentLengthStack.at(-1) /* peek() */;
        if (curIndentLength > prevIndentLength) {
            this.#createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, null, this.ffgToken);
            this.indentLengthStack.push(curIndentLength);
        } else {
            while (curIndentLength < prevIndentLength) { // more than 1 DEDENT token may be inserted to the token stream
                this.indentLengthStack.pop();
                prevIndentLength = this.indentLengthStack.at(-1) /* peek() */;
                if (curIndentLength <= prevIndentLength) {
                    this.#createAndAddPendingToken(PythonLexer.DEDENT, Token.DEFAULT_CHANNEL, null, this.ffgToken);
                } else {
                    this.#reportError("inconsistent dedent");
                }
            }
        }
    }

    #handleFSTRING_MIDDLE_token() { // replace the double braces '{{' or '}}' to single braces and hide the second braces
        let fsMid = this.curToken.text;
        fsMid = fsMid.replaceAll(/\{\{/g, "{_").replaceAll(/\}\}/g, "}_"); // replace: {{ --> {_  and   }} --> }_
        let arrOfStr = fsMid.split(/(?<=[{}])_/); // split by {_  or  }_
        for (let s of arrOfStr) {
            if (s) {
                this.#createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, s, this.ffgToken);
                let lastCharacter = s.charAt(s.length - 1);
                if ("{}".includes(lastCharacter)) {
                    this.#createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.HIDDEN_CHANNEL, lastCharacter, this.ffgToken);
                    // this inserted hidden token allows to restore the original f-string literal with the double braces
                }
            }
        }
    }

    #handleFStringLexerModes() { // https://peps.python.org/pep-0498/#specification
        if (this._modeStack.length > 0) {
            switch (this.curToken.type) {
                case PythonLexer.LBRACE:
                    this.pushMode(Lexer.DEFAULT_MODE);
                    this.paren_or_bracket_openedStack.push(0);
                    break;
                case PythonLexer.LPAR:
                case PythonLexer.LSQB:
                    // https://peps.python.org/pep-0498/#lambdas-inside-expressions
                    this.paren_or_bracket_openedStack.push(this.paren_or_bracket_openedStack.pop + 1); // increment the last element
                    break;
                case PythonLexer.RPAR:
                case PythonLexer.RSQB:
                    this.paren_or_bracket_openedStack.push(this.paren_or_bracket_openedStack.pop - 1); // decrement the last element
                    break;
                case PythonLexer.COLON: // colon can only come from DEFAULT_MODE
                    if (this.paren_or_bracket_openedStack.at(-1) /* peek() */ == 0) {
                        switch (this._modeStack.at(-1) /* peek() */) { // check the previous lexer mode (the current is DEFAULT_MODE)
                            case PythonLexer.SINGLE_QUOTE_FSTRING_MODE:
                            case PythonLexer.LONG_SINGLE_QUOTE_FSTRING_MODE:
                            case PythonLexer.SINGLE_QUOTE_FORMAT_SPECIFICATION_MODE:
                                this.setMode(PythonLexer.SINGLE_QUOTE_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                                break;
                            case PythonLexer.DOUBLE_QUOTE_FSTRING_MODE:
                            case PythonLexer.LONG_DOUBLE_QUOTE_FSTRING_MODE:
                            case PythonLexer.DOUBLE_QUOTE_FORMAT_SPECIFICATION_MODE:
                                this.setMode(PythonLexer.DOUBLE_QUOTE_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                                break;
                        }
                    }
                    break;
                case PythonLexer.RBRACE:
                    switch (this._mode) {
                        case Lexer.DEFAULT_MODE:
                        case PythonLexer.SINGLE_QUOTE_FORMAT_SPECIFICATION_MODE:
                        case PythonLexer.DOUBLE_QUOTE_FORMAT_SPECIFICATION_MODE:
                            this.popMode();
                            this.paren_or_bracket_openedStack.pop();
                            break;
                        default:
                            this.#reportLexerError("f-string: single '}' is not allowed");
                            break;
                    }
                    break;
            }
        }
    }

    #handleFORMAT_SPECIFICATION_MODE() {
        if (this._modeStack.length > 0 && this.ffgToken.type === PythonLexer.RBRACE) {
            switch (this.curToken.type) {
                case PythonLexer.COLON:
                case PythonLexer.RBRACE:
                    // insert an empty FSTRING_MIDDLE token instead of the missing format specification
                    this.#createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, "", this.ffgToken);
                    break;
            }
        }
    }

    #insertTrailingTokens() {
        switch (this.lastPendingTokenTypeFromDefaultChannel) {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default:
                // insert an extra trailing NEWLINE token that serves as the end of the last statement
                this.#createAndAddPendingToken(PythonLexer.NEWLINE, Token.DEFAULT_CHANNEL, null, this.ffgToken); // _ffgToken is EOF
        }
        this.#insertIndentOrDedentToken(0); // Now insert as much trailing DEDENT tokens as needed
    }

    #handleEOFtoken() {
        if (this.lastPendingTokenTypeFromDefaultChannel > 0) {
            // there was a statement in the input (leading NEWLINE tokens are hidden)
            this.#insertTrailingTokens();
        }
        this.#addPendingToken(this.curToken);
    }

    #hideAndAddPendingToken(ctkn) {
        ctkn.channel = Token.HIDDEN_CHANNEL;
        this.#addPendingToken(ctkn);
    }

    #createAndAddPendingToken(type, channel, text, sampleToken) {
        const ctkn = sampleToken.clone();
        ctkn.type = type;
        ctkn.channel = channel;
        ctkn.stop = sampleToken.start - 1;
        ctkn.text = text == null ?
            `<${this.getSymbolicNames()[type]}>` :
            text;

        this.#addPendingToken(ctkn);
    }

    #addPendingToken(tkn) {
        // save the last pending token type because the _pendingTokens linked list can be empty by the nextToken()
        this.previousPendingTokenType = tkn.type;
        if (tkn.channel === Token.DEFAULT_CHANNEL) {
            this.lastPendingTokenTypeFromDefaultChannel = this.previousPendingTokenType;
        }
        this.pendingTokens.push(tkn) /* .addLast(token) */;
    }

    #getIndentationLength(indentText) { // the indentText may contain spaces, tabs or form feeds
        const TAB_LENGTH = 8; // the standard number of spaces to replace a tab to spaces
        let length = 0;
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
                length = this.#INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    #reportLexerError(errMsg) {
        this.getErrorListener().syntaxError(this, this.curToken, this.curToken.line, this.curToken.column, " LEXER" + this.#ERR_TXT + errMsg, null);
    }

    #reportError(errMsg) {
        this.#reportLexerError(errMsg);

        // the ERRORTOKEN will raise an error in the parser
        this.#createAndAddPendingToken(PythonLexer.ERRORTOKEN, Token.DEFAULT_CHANNEL, this.#ERR_TXT + errMsg, this.ffgToken);
    }
}
