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

import { CharStreams, CommonTokenStream, Token, CommonToken, Lexer } from "antlr4";
import PythonLexer  from "./PythonLexer.js";
import PythonParser from "./PythonParser.js";

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
        // A stack that stores expression(s) between braces in fstring
        this.braceExpressionStack;
        this.prevBraceExpression;

        // Instead of this._mode      (_mode is not implemented in each ANTLR4 runtime)
        this.curLexerMode;
        // Instead of this._modeStack (_modeStack is not implemented in each ANTLR4 runtime)
        this.lexerModeStack;

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
        return this.pendingTokens.shift() /* stack pollFirst() */; // add the queued token to the token stream
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
        this.braceExpressionStack = [];
        this.prevBraceExpression = "";
        this.curLexerMode = 0;
        this.lexerModeStack = [];
        this.wasSpaceIndentation = false;
        this.wasTabIndentation = false;
        this.wasIndentationMixedWithSpacesAndTabs = false;
        this.curToken = null;
        this.ffgToken = null;
    }

    #checkNextToken() {
        if (this.previousPendingTokenType === Token.EOF)
            return;

        if (this.indentLengthStack.length === 0) { // We're at the first token
            this.#insertENCODINGtoken();
            this.#setCurrentAndFollowingTokens();
            this.#handleStartOfInput();
        } else {
            this.#setCurrentAndFollowingTokens();
        }

        switch (this.curToken.type) {
            case PythonLexer.NEWLINE:
                this.#handleNEWLINEtoken();
                break;
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
            case PythonLexer.FSTRING_MIDDLE:
                this.#handleFSTRING_MIDDLEtokenWithDoubleBrace(); // does not affect the opened field
                this.#addPendingToken(this.curToken);
                break;
            case PythonLexer.COLONEQUAL:
                this.#handleCOLONEQUALtokenInFString();
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

    #setCurrentAndFollowingTokens() {
        this.curToken = this.ffgToken == undefined ?
            super.nextToken() :
            this.ffgToken;

        this.#checkCurToken(); // ffgToken cannot be used in this method and its sub methods (ffgToken is not yet set)!

        this.ffgToken = this.curToken.type === Token.EOF ?
            this.curToken :
            super.nextToken();
    }

    #insertENCODINGtoken() {
        let lineBuilder = [];
        let encodingName = "";
        let lineCount = 0;
        const ws_commentPattern = /^[ \t\f]*(#.*)?$/;
        const inputStream = this.inputStream;
        const size = inputStream.size;

        inputStream.seek(0);
        for (let i = 0; i < size; i++) {
            let c = String.fromCharCode(inputStream.LA(i + 1));
            lineBuilder.push(c);

            if (c == '\n' || i == size - 1) {
                let line = lineBuilder.join("").replace("\r", "").replace("\n", "");
                if (ws_commentPattern.test(line)) { // WS* + COMMENT? found
                    encodingName = this.#getEncodingName(line);
                    if (encodingName !== "") {
                        break; // encoding found
                    }
                } else {
                    break; // statement or backslash found (line is not empty, not whitespace, not comment)
                }

                lineCount++;
                if (lineCount >= 2) {
                    break; // check only the first two lines
                }
                lineBuilder = [];
            }
        }

        if (encodingName === "") {
            encodingName = "utf-8"; // default Python source code encoding
        }

        const encodingToken = new CommonToken([null, null], PythonLexer.ENCODING, Token.HIDDEN_CHANNEL, 0, 0);
        encodingToken.text = encodingName;
        encodingToken.line = 0;
        encodingToken.column = -1;
        this.#addPendingToken(encodingToken);
    }

    #getEncodingName(commentText) { // https://peps.python.org/pep-0263/#defining-the-encoding
        const encodingCommentPattern = /^[ \t\f]*#.*?coding[:=][ \t]*([-_.a-zA-Z0-9]+)/;
        const match = commentText.match(encodingCommentPattern);
        return match ? match[1] : "";
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
            const prevToken = this.pendingTokens.at(- 1); /* stack peekLast() */ // WS token
            if (this.#getIndentationLength(prevToken.text) !== 0) { // there is an "indentation" before the first statement
                const errMsg = "first statement indented";
                this.#reportLexerError(errMsg);
                // insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
                this.#createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, this.#ERR_TXT + errMsg, this.curToken);
            }
        }
    }

    #handleNEWLINEtoken() {
        if (this.lexerModeStack.length > 0) {
            this.#addPendingToken(this.curToken);
        } else if (this.opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
            this.#hideAndAddPendingToken(this.curToken);
        } else {
            const nlToken = this.curToken.clone(); // save the current NEWLINE token
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
                    if (isLookingAhead) { // We're on a whitespace(s) followed by a statement
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

    #checkCurToken() {
        switch (this.curToken.type) {
            case PythonLexer.FSTRING_START:
                this.#setLexerModeByFSTRING_STARTtoken();
                return;
            case PythonLexer.FSTRING_MIDDLE:
                this.#handleFSTRING_MIDDLEtokenWithQuoteAndLBrace(); // affect the opened field
                if (this.curToken.type === PythonLexer.FSTRING_MIDDLE) {
                    return; // No curToken exchange happened
                }
                break;
            case PythonLexer.FSTRING_END:
                this.#popLexerMode();
                return;
            default:
                if (this.lexerModeStack.length === 0) {
                    return; // Not in fstring mode
                }
        }

        switch (this.curToken.type) { // the following tokens can only come from default mode (after an LBRACE in fstring)
            case PythonLexer.NEWLINE:
                // append the current brace expression with the current newline
                this.#appendToBraceExpression(this.curToken.text)
                this.curToken.channel = Token.HIDDEN_CHANNEL;
                break;
            case PythonLexer.LBRACE:
                // the outermost brace expression cannot be a dictionary comprehension or a set comprehension
                this.braceExpressionStack.push("{");
                this.paren_or_bracket_openedStack.push(0);
                this.#pushLexerMode(Lexer.DEFAULT_MODE);
                break;
            case PythonLexer.LPAR:
            case PythonLexer.LSQB:
                // append the current brace expression with a "(" or a "["
                this.#appendToBraceExpression(this.curToken.text)
                // https://peps.python.org/pep-0498/#lambdas-inside-expressions
                this.#incrementBraceStack();
                break;
            case PythonLexer.RPAR:
            case PythonLexer.RSQB:
                // append the current brace expression with a ")" or a "]"
                this.#appendToBraceExpression(this.curToken.text)
                this.#decrementBraceStack();
                break;
            case PythonLexer.COLON:
            case PythonLexer.COLONEQUAL:
                // append the current brace expression with a ":" or a ":="
                this.#appendToBraceExpression(this.curToken.text)
                this.#setLexerModeByCOLONorCOLONEQUALtoken();
                break;
            case PythonLexer.RBRACE:
                this.#setLexerModeAfterRBRACEtoken();
                break;
            default:
                // append the current brace expression with the current token text
                this.#appendToBraceExpression(this.curToken.text)
        }
    }

    #appendToBraceExpression(text) {
        this.braceExpressionStack[this.braceExpressionStack.length - 1] += text;
    }

    #incrementBraceStack() { // increment the last element (peek() + 1)
        this.paren_or_bracket_openedStack[this.paren_or_bracket_openedStack.length - 1]++;
    }

    #decrementBraceStack() { // decrement the last element (peek() - 1)
        this.paren_or_bracket_openedStack[this.paren_or_bracket_openedStack.length - 1]--;
    }

    #setLexerModeAfterRBRACEtoken() {
        switch (this.curLexerMode) {
            case Lexer.DEFAULT_MODE:
                this.#popLexerMode();
                this.#popByBRACE();
                break;
            case PythonLexer.SQ1__FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ1R_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1__FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1R_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3__FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3R_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3__FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3R_FORMAT_SPECIFICATION_MODE:
                this.#popLexerMode();
                this.#popLexerMode();
                this.#popByBRACE();
                break;
            default:
                this.#reportLexerError("f-string: single '}' is not allowed");
        }
    }

    #setLexerModeByFSTRING_STARTtoken() {
        const text = this.curToken.text.toLowerCase();
        const modeMap = {
            "f'": PythonLexer.SQ1__FSTRING_MODE,
            "rf'": PythonLexer.SQ1R_FSTRING_MODE,
            "fr'": PythonLexer.SQ1R_FSTRING_MODE,
            'f"': PythonLexer.DQ1__FSTRING_MODE,
            'rf"': PythonLexer.DQ1R_FSTRING_MODE,
            'fr"': PythonLexer.DQ1R_FSTRING_MODE,
            "f'''": PythonLexer.SQ3__FSTRING_MODE,
            "rf'''": PythonLexer.SQ3R_FSTRING_MODE,
            "fr'''": PythonLexer.SQ3R_FSTRING_MODE,
            'f"""': PythonLexer.DQ3__FSTRING_MODE,
            'rf"""': PythonLexer.DQ3R_FSTRING_MODE,
            'fr"""': PythonLexer.DQ3R_FSTRING_MODE,
        };
        const mode = modeMap[text];
        if (mode !== undefined) {
            this.#pushLexerMode(mode);
        }
    }

    #setLexerModeByCOLONorCOLONEQUALtoken() {
        if (this.paren_or_bracket_openedStack[this.paren_or_bracket_openedStack.length - 1] === 0) { // stack peek == 0
            const previousMode = this.lexerModeStack[this.lexerModeStack.length - 1]; // stack peek
            switch (previousMode) { // check the previous lexer mode (the current is DEFAULT_MODE)
                case PythonLexer.SQ1__FSTRING_MODE:
                case PythonLexer.SQ1__FORMAT_SPECIFICATION_MODE:
                    this.#pushLexerMode(PythonLexer.SQ1__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.SQ1R_FSTRING_MODE:
                case PythonLexer.SQ1R_FORMAT_SPECIFICATION_MODE:
                    this.#pushLexerMode(PythonLexer.SQ1R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.DQ1__FSTRING_MODE:
                case PythonLexer.DQ1__FORMAT_SPECIFICATION_MODE:
                    this.#pushLexerMode(PythonLexer.DQ1__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.DQ1R_FSTRING_MODE:
                case PythonLexer.DQ1R_FORMAT_SPECIFICATION_MODE:
                    this.#pushLexerMode(PythonLexer.DQ1R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.SQ3__FSTRING_MODE:
                case PythonLexer.SQ3__FORMAT_SPECIFICATION_MODE:
                    this.#pushLexerMode(PythonLexer.SQ3__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.SQ3R_FSTRING_MODE:
                case PythonLexer.SQ3R_FORMAT_SPECIFICATION_MODE:
                    this.#pushLexerMode(PythonLexer.SQ3R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.DQ3__FSTRING_MODE:
                case PythonLexer.DQ3__FORMAT_SPECIFICATION_MODE:
                    this.#pushLexerMode(PythonLexer.DQ3__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.DQ3R_FSTRING_MODE:
                case PythonLexer.DQ3R_FORMAT_SPECIFICATION_MODE:
                    this.#pushLexerMode(PythonLexer.DQ3R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;                
            }
        }
    }

    #popByBRACE() {
        this.paren_or_bracket_openedStack.pop();
        this.prevBraceExpression = this.braceExpressionStack.pop() + "}";
        if (this.braceExpressionStack.length > 0) {
            // append the current brace expression with the previous brace expression
            this.braceExpressionStack[this.braceExpressionStack.length - 1] += this.prevBraceExpression;
        }
    }

    #handleFSTRING_MIDDLEtokenWithDoubleBrace() {
        // replace the trailing double brace with a single brace and insert a hidden brace token
        switch (this.#getLastTwoCharsOfTheCurTokenText()) {
            case "{{":
                this.#trimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", Token.HIDDEN_CHANNEL);
                break;
            case "}}":
                this.#trimLastCharAddPendingTokenSetCurToken(PythonLexer.RBRACE, "}", Token.HIDDEN_CHANNEL);
                break;
        }
    }

    #handleFSTRING_MIDDLEtokenWithQuoteAndLBrace() {
        // replace the trailing     quote + left_brace with a quote     and insert an LBRACE token
        // replace the trailing backslash + left_brace with a backslash and insert an LBRACE token        
        switch (this.#getLastTwoCharsOfTheCurTokenText()) {
            case "\"{":
            case "'{":
            case "\\{":
                this.#trimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", Token.DEFAULT_CHANNEL);
                break;
        }
    }

    #getLastTwoCharsOfTheCurTokenText() {
        return this.curToken.text.slice(-2);
    }

    #trimLastCharAddPendingTokenSetCurToken(type, text, channel) {
        // trim the last char and add the modified curToken to the pendingTokens stack
        const tokenTextWithoutLastChar = this.curToken.text.slice(0, -1);
        this.curToken.text = tokenTextWithoutLastChar;
        this.curToken.stop -= 1;
        this.#addPendingToken(this.curToken);

        this.#createNewCurToken(type, text, channel); // set curToken
    }

    #handleCOLONEQUALtokenInFString() {
        if (this.lexerModeStack.length > 0 &&
            this.paren_or_bracket_openedStack[this.paren_or_bracket_openedStack.length - 1] === 0) { // stack peek == 0

            // In fstring a colonequal (walrus operator) can only be used in parentheses
            // Not in parentheses, replace COLONEQUAL token with COLON as format specifier
            // and insert the equal symbol to the following FSTRING_MIDDLE token
            this.curToken.type = PythonLexer.COLON;
            this.curToken.text = ":";
            this.curToken.stop = this.curToken.start;

            if (this.ffgToken.type === PythonLexer.FSTRING_MIDDLE) {
                this.ffgToken.text = "=" + this.ffgToken.text;
                this.ffgToken.start -= 1;
                this.ffgToken.column -= 1;
            } else {
                this.#addPendingToken(this.curToken);
                this.#createNewCurToken(PythonLexer.FSTRING_MIDDLE, "=", Token.DEFAULT_CHANNEL);
            }
        }
        this.#addPendingToken(this.curToken);
    }

    #createNewCurToken(type, text, channel) {
        const ctkn = this.curToken.clone();
        ctkn.type = type;
        ctkn.text = text;
        ctkn.channel = channel;
        ctkn.column += 1;
        ctkn.start += 1;
        ctkn.stop = ctkn.start;
        this.curToken = ctkn;
    }

    #pushLexerMode(mode) {
        this.pushMode(mode);
        this.lexerModeStack.push(this.curLexerMode);
        this.curLexerMode = mode;
    }

    #popLexerMode() {
        this.popMode();
        this.curLexerMode = this.lexerModeStack.pop();
    }

    #handleFORMAT_SPECIFICATION_MODE() {
        if (this.lexerModeStack.length > 0 &&
            this.ffgToken.type === PythonLexer.RBRACE) {

            // insert an empty FSTRING_MIDDLE token instead of the missing format specification
            switch (this.curToken.type) {
                case PythonLexer.COLON:
                    this.#createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, "", this.ffgToken);
                    break;
                case PythonLexer.RBRACE:
                    // only if the previous brace expression is not a dictionary comprehension or set comprehension
                    if (!this.#isDictionaryComprehensionOrSetComprehension(this.prevBraceExpression)) {
                        this.#createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, "", this.ffgToken);
                    }
                    break;
            }
        }
    }

    #isDictionaryComprehensionOrSetComprehension(code) {
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
