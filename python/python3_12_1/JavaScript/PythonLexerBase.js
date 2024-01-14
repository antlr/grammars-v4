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

import antlr4, { Token } from "antlr4";
import PythonLexer from "./PythonLexer.js";

export default class PythonLexerBase extends antlr4.Lexer {
    constructor(input) {
        super(input);

        // A stack that keeps track of the indentation lengths
        this._indentLengthStack;
        // A list where tokens are waiting to be loaded into the token stream
        this._pendingTokens;

        // last pending token types
        this._previousPendingTokenType;
        this._lastPendingTokenTypeFromDefaultChannel;

        // The amount of opened parentheses, square brackets or curly braces
        this._opened;
        //  The amount of opened parentheses and square brackets in the current lexer mode
        this._paren_or_bracket_openedStack;

        this._wasSpaceIndentation;
        this._wasTabIndentation;
        this._wasIndentationMixedWithSpacesAndTabs;
        this._INVALID_LENGTH;
        
        this._curToken; // current (under processing) token
        this._ffgToken; // following (look ahead) token

        this._ERR_TXT;

        this.init();
    }

    init() {
        this._indentLengthStack = [];
        this._pendingTokens = [];
        this._previousPendingTokenType = 0;
        this._lastPendingTokenTypeFromDefaultChannel = 0;
        this._opened = 0;
        this._paren_or_bracket_openedStack = [];
        this._wasSpaceIndentation = false;
        this._wasTabIndentation = false;
        this._wasIndentationMixedWithSpacesAndTabs = false;
        this._INVALID_LENGTH = -1;
        this._curToken = null;
        this._ffgToken = null;
        this._ERR_TXT = " ERROR: ";        
    }

    nextToken() { // reading the input stream until a return EOF
        this.checkNextToken();
        return this._pendingTokens.shift() /* .pollFirst() */; // add the queued token to the token stream
    }

    checkNextToken() {
        if (this._previousPendingTokenType !== PythonLexer.EOF) {
            this.setCurrentAndFollowingTokens();
            if (this._indentLengthStack.length === 0) { // We're at the first token
                this.handleStartOfInput();
            }

            switch (this._curToken.type) {
                case PythonLexer.LPAR:
                case PythonLexer.LSQB:
                case PythonLexer.LBRACE:
                    this._opened++;
                    this.addPendingToken(this._curToken);
                    break;
                case PythonLexer.RPAR:
                case PythonLexer.RSQB:
                case PythonLexer.RBRACE:
                    this._opened--;
                    this.addPendingToken(this._curToken);
                    break;
                case PythonLexer.NEWLINE:
                    this.handleNEWLINEtoken();
                    break;
                case PythonLexer.STRING:
                    this.handleSTRINGtoken();
                    break;
                case PythonLexer.FSTRING_MIDDLE:
                    this.handleFSTRING_MIDDLE_token();
                    break;
                case PythonLexer.ERROR_TOKEN:
                    this.reportLexerError(`token recognition error at: '${this._curToken.text}'`);
                    this.addPendingToken(this._curToken);
                    break;
                case PythonLexer.EOF:
                    this.handleEOFtoken();
                    break;
                default:
                    this.addPendingToken(this._curToken);
            }
            this.handleFORMAT_SPECIFICATION_MODE();
        }
    }

    setCurrentAndFollowingTokens() {
        this._curToken = this._ffgToken == undefined ?
            this.getCommonTokenByToken(super.nextToken()) :
            this.getCommonTokenByToken(this._ffgToken);

        this.handleFStringLexerModes();

        this._ffgToken = this._curToken.type === PythonLexer.EOF ?
            this._curToken :
            this.getCommonTokenByToken(super.nextToken());
    }

    // initialize the _indentLengthStack
    // hide the leading NEWLINE token(s)
    // if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    // insert a leading INDENT token if necessary
    handleStartOfInput() {
        // initialize the stack with a default 0 indentation length
        this._indentLengthStack.push(0); // this will never be popped off
        while (this._curToken.type !== PythonLexer.EOF) {
            if (this._curToken.channel === Token.DEFAULT_CHANNEL) {
                if (this._curToken.type === PythonLexer.NEWLINE) {
                    // all the NEWLINE tokens must be ignored before the first statement
                    this.hideAndAddPendingToken(this._curToken);
                } else { // We're at the first statement
                    this.insertLeadingIndentToken();
                    return; // continue the processing of the current token with checkNextToken()
                }
            } else {
                this.addPendingToken(this._curToken); // it can be WS, EXPLICIT_LINE_JOINING or COMMENT token
            }
            this.setCurrentAndFollowingTokens();
        } // continue the processing of the EOF token with checkNextToken()
    }

    insertLeadingIndentToken() {
        if (this._previousPendingTokenType === PythonLexer.WS) {
            let prevToken = this._pendingTokens.at(- 1) /* .peekLast() */; // WS token
            if (this.getIndentationLength(prevToken.text) !== 0) { // there is an "indentation" before the first statement
                const errMsg = "first statement indented";
                this.reportLexerError(errMsg);
                // insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
                this.createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, this._ERR_TXT + errMsg, this._curToken);
            }
        }
    }

    handleNEWLINEtoken() {
        if (this._opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
            this.hideAndAddPendingToken(this._curToken);
        } else {
            let nlToken = this._curToken; // save the current NEWLINE token
            const isLookingAhead = this._ffgToken.type === PythonLexer.WS;
            if (isLookingAhead) {
                this.setCurrentAndFollowingTokens(); // set the next two tokens
            }

            switch (this._ffgToken.type) {
                case PythonLexer.NEWLINE:      // We're before a blank line
                case PythonLexer.COMMENT:      // We're before a comment
                case PythonLexer.TYPE_COMMENT: // We're before a type comment
                    this.hideAndAddPendingToken(nlToken);
                    if (isLookingAhead) {
                        this.addPendingToken(this._curToken);  // WS token
                    }
                    break;
                default:
                    this.addPendingToken(nlToken);
                    if (isLookingAhead) { // We're on whitespace(s) followed by a statement
                        const indentationLength = this._ffgToken.type === PythonLexer.EOF ?
                                                  0 :
                                                  this.getIndentationLength(this._curToken.text);

                        if (indentationLength !== this._INVALID_LENGTH) {
                            this.addPendingToken(this._curToken); // WS token
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
        let prevIndentLength = this._indentLengthStack.at(-1) /* peek() */;
        if (curIndentLength > prevIndentLength) {
            this.createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, null, this._ffgToken);
            this._indentLengthStack.push(curIndentLength);
        } else {
            while (curIndentLength < prevIndentLength) { // more than 1 DEDENT token may be inserted to the token stream
                this._indentLengthStack.pop();
                prevIndentLength = this._indentLengthStack.at(-1) /* peek() */;
                if (curIndentLength <= prevIndentLength) {
                    this.createAndAddPendingToken(PythonLexer.DEDENT, Token.DEFAULT_CHANNEL, null, this._ffgToken);
                } else {
                    this.reportError("inconsistent dedent");
                }
            }
        }
    }

    handleSTRINGtoken() { // remove the \<newline> escape sequences from the string literal
        let line_joinFreeStringLiteral = this._curToken.text.replace(/\\(\r?\n)/g, "");
        if (this._curToken.text.length === line_joinFreeStringLiteral.length) {
            this.addPendingToken(this._curToken);
        } else {
            let originalSTRINGtoken = this.getCommonTokenByToken(this._curToken); // backup the original token
            this._curToken.text = line_joinFreeStringLiteral;
            this.addPendingToken(this._curToken); // add the modified token with inline string literal
            this.hideAndAddPendingToken(originalSTRINGtoken); // add the original token to the hidden channel
            // this inserted hidden token allows to restore the original string literal with the \<newline> escape sequences
        }
    }

    handleFSTRING_MIDDLE_token() { // replace the double braces '{{' or '}}' to single braces and hide the second braces
        let fsMid = this._curToken.text;
        fsMid = fsMid.replaceAll(/\{\{/g, "{_").replaceAll(/\}\}/g, "}_"); // replace: {{ --> {_    }} --> }_
        let arrOfStr = fsMid.split(/(?<=[{}])_/); // split by {_  or  }_
        for (let s of arrOfStr) {
            if (s) {
                this.createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, s, this._ffgToken);
                let lastCharacter = s.charAt(s.length - 1);
                if ("{}".includes(lastCharacter)) {
                    this.createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.HIDDEN_CHANNEL, lastCharacter, this._ffgToken);
                    // this inserted hidden token allows to restore the original f-string literal with the double braces
                }
            }
        }
    }

    handleFStringLexerModes() { // https://peps.python.org/pep-0498/#specification
        if (this._modeStack.length > 0) {
            switch (this._curToken.type) {
                case PythonLexer.LBRACE:
                    this.pushMode(PythonLexer.DEFAULT_MODE);
                    this._paren_or_bracket_openedStack.push(0);
                    break;
                case PythonLexer.LPAR:
                case PythonLexer.LSQB:
                    // https://peps.python.org/pep-0498/#lambdas-inside-expressions
                    this._paren_or_bracket_openedStack.push(this._paren_or_bracket_openedStack.pop + 1); // increment the last element
                    break;
                case PythonLexer.RPAR:
                case PythonLexer.RSQB:
                    this._paren_or_bracket_openedStack.push(this._paren_or_bracket_openedStack.pop - 1); // decrement the last element
                    break;
                case PythonLexer.COLON: // colon can only come from DEFAULT_MODE
                    if (this._paren_or_bracket_openedStack.at(-1) /* peek() */ == 0) {
                        switch (this._modeStack.at(-1) /* peek() */) { // check the previous lexer mode (the current is DEFAULT_MODE)
                            case PythonLexer.SINGLE_QUOTE_FSTRING_MODE:
                            case PythonLexer.LONG_SINGLE_QUOTE_FSTRING_MODE:
                            case PythonLexer.SINGLE_QUOTE_FORMAT_SPECIFICATION_MODE:
                                this.mode(PythonLexer.SINGLE_QUOTE_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                                break;
                            case PythonLexer.DOUBLE_QUOTE_FSTRING_MODE:
                            case PythonLexer.LONG_DOUBLE_QUOTE_FSTRING_MODE:
                            case PythonLexer.DOUBLE_QUOTE_FORMAT_SPECIFICATION_MODE:
                                this.mode(PythonLexer.DOUBLE_QUOTE_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                                break;
                        }
                    }
                    break;
                case PythonLexer.RBRACE:
                    switch (this._mode) {
                        case PythonLexer.DEFAULT_MODE:
                        case PythonLexer.SINGLE_QUOTE_FORMAT_SPECIFICATION_MODE:
                        case PythonLexer.DOUBLE_QUOTE_FORMAT_SPECIFICATION_MODE:
                            this.popMode();
                            this._paren_or_bracket_openedStack.pop();
                            break;
                        default:
                            this.reportLexerError("f-string: single '}' is not allowed");
                            break;
                    }
                    break;
            }
        }
    }

    handleFORMAT_SPECIFICATION_MODE() {
        if (this._modeStack.length > 0 && this._ffgToken.type === PythonLexer.RBRACE) {
            switch (this._curToken.type) {
                case PythonLexer.COLON:
                case PythonLexer.RBRACE:
                    // insert an empty FSTRING_MIDDLE token instead of the missing format specification
                    this.createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, "", this._ffgToken);
                    break;
            }
        }
    }

    insertTrailingTokens() {
        switch (this._lastPendingTokenTypeFromDefaultChannel) {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default:
                // insert an extra trailing NEWLINE token that serves as the end of the last statement
                this.createAndAddPendingToken(PythonLexer.NEWLINE, Token.DEFAULT_CHANNEL, null, this._ffgToken); // _ffgToken is EOF
        }
        this.insertIndentOrDedentToken(0); // Now insert as much trailing DEDENT tokens as needed
    }

    handleEOFtoken() {
        if (this._lastPendingTokenTypeFromDefaultChannel > 0) {
            // there was a statement in the input (leading NEWLINE tokens are hidden)
            this.insertTrailingTokens();
        }
        this.addPendingToken(this._curToken);
    }

    hideAndAddPendingToken(token) {
        token.channel = Token.HIDDEN_CHANNEL;
        this.addPendingToken(token);
    }

    createAndAddPendingToken(type, channel, text, baseToken) {
        const token = this.getCommonTokenByToken(baseToken);
        token.type = type;
        token.channel = channel;
        token.stop = baseToken.start - 1;
        token.text = text == null ?
            `<${this.getSymbolicNames()[type]}>` :
            text;

        this.addPendingToken(token);
    }

    addPendingToken(token) {
        // save the last pending token type because the _pendingTokens linked list can be empty by the nextToken()
        this._previousPendingTokenType = token.type;
        if (token.channel === Token.DEFAULT_CHANNEL) {
            this._lastPendingTokenTypeFromDefaultChannel = this._previousPendingTokenType;
        }
        this._pendingTokens.push(token) /* .addLast(token) */;
    }

    getCommonTokenByToken(baseToken) {
        let commonToken = new antlr4.CommonToken(baseToken.source, baseToken.type, baseToken.channel, baseToken.start, baseToken.stop);
        commonToken.tokenIndex = baseToken.tokenIndex;
        commonToken.line = baseToken.line;
        commonToken.column = baseToken.column;
        commonToken.text = baseToken.text;
        return commonToken;
    }

    getIndentationLength(textWS) { // the textWS may contain spaces, tabs or formfeeds
        const TAB_LENGTH = 8; // the standard number of spaces to replace a tab to spaces
        let length = 0;

        for (let ch of textWS) {
            switch (ch) {
                case " ":
                    this._wasSpaceIndentation = true;
                    length += 1;
                    break;
                case "\t":
                    this._wasTabIndentation = true;
                    length += TAB_LENGTH - (length % TAB_LENGTH);
                    break;
                case "\f": // formfeed
                    length = 0;
                    break;
            }
        }

        if (this._wasTabIndentation && this._wasSpaceIndentation) {
            if (!this._wasIndentationMixedWithSpacesAndTabs) {
                this._wasIndentationMixedWithSpacesAndTabs = true;
                return this._INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    reportLexerError(errMsg) {
        this.getErrorListenerDispatch().syntaxError(this, this._curToken, this._curToken.line, this._curToken.column, " LEXER" + this._ERR_TXT + errMsg, null);
    }

    reportError(errMsg) {
        this.reportLexerError(errMsg);

        // the ERROR_TOKEN will raise an error in the parser
        this.createAndAddPendingToken(PythonLexer.ERROR_TOKEN, Token.DEFAULT_CHANNEL, this._ERR_TXT + errMsg, this._ffgToken);
    }

    reset() {
        this.init();
        super.reset();
    }
}
