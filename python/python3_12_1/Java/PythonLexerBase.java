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

import java.util.*;

import org.antlr.v4.runtime.*;

public abstract class PythonLexerBase extends Lexer {
    // A stack that keeps track of the indentation lengths
    private Deque<Integer> indentLengthStack;
    // A list where tokens are waiting to be loaded into the token stream
    private LinkedList<Token> pendingTokens;

    // last pending token types
    private int previousPendingTokenType;
    private int lastPendingTokenTypeFromDefaultChannel;

    // The amount of opened parentheses, square brackets or curly braces
    private int opened;
    //  The amount of opened parentheses and square brackets in the current lexer mode
    private Deque<Integer> paren_or_bracket_openedStack;

    private boolean wasSpaceIndentation;
    private boolean wasTabIndentation;
    private boolean wasIndentationMixedWithSpacesAndTabs;
    private final int INVALID_LENGTH = -1;

    private CommonToken curToken; // current (under processing) token
    private Token ffgToken; // following (look ahead) token

    private final String ERR_TXT = " ERROR: ";

    protected PythonLexerBase(CharStream input) {
        super(input);
        this.init();
    }

    private void init() {
        this.indentLengthStack = new ArrayDeque<>();
        this.pendingTokens = new LinkedList<>();
        this.previousPendingTokenType = 0;
        this.lastPendingTokenTypeFromDefaultChannel = 0;
        this.opened = 0;
        this.paren_or_bracket_openedStack = new ArrayDeque<>();
        this.wasSpaceIndentation = false;
        this.wasTabIndentation = false;
        this.wasIndentationMixedWithSpacesAndTabs = false;
        this.curToken = null;
        this.ffgToken = null;
    }

    @Override
    public Token nextToken() { // reading the input stream until a return EOF
        this.checkNextToken();
        return this.pendingTokens.pollFirst(); // add the queued token to the token stream
    }

    private void checkNextToken() {
        if (this.previousPendingTokenType != Token.EOF) {
            this.setCurrentAndFollowingTokens();
            if (this.indentLengthStack.isEmpty()) { // We're at the first token
                this.handleStartOfInput();
            }

            switch (this.curToken.getType()) {
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
                case PythonLexer.FSTRING_MIDDLE:
                    this.handleFSTRING_MIDDLE_token();
                    break;
                case PythonLexer.ERROR_TOKEN:
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
    }

    private void setCurrentAndFollowingTokens() {
        this.curToken = this.ffgToken == null ?
                        new CommonToken(super.nextToken()) :
                        new CommonToken(this.ffgToken);

        this.handleFStringLexerModes();

        this.ffgToken = this.curToken.getType() == Token.EOF ?
                        this.curToken :
                        super.nextToken();
    }

    // initialize the indentLengthStack
    // hide the leading NEWLINE token(s)
    // if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    // insert a leading INDENT token if necessary
    private void handleStartOfInput() {
        // initialize the stack with a default 0 indentation length
        this.indentLengthStack.push(0); // this will never be popped off
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
        } // continue the processing of the EOF token with checkNextToken()
    }

    private void insertLeadingIndentToken() {
        if (this.previousPendingTokenType == PythonLexer.WS) {
            Token prevToken = this.pendingTokens.peekLast(); // WS token
            if (this.getIndentationLength(prevToken.getText()) != 0) { // there is an "indentation" before the first statement
                final String errMsg = "first statement indented";
                this.reportLexerError(errMsg);
                // insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
                this.createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, this.ERR_TXT + errMsg, this.curToken);
            }
        }
    }

    private void handleNEWLINEtoken() {
        if (this.opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
            this.hideAndAddPendingToken(this.curToken);
        } else {
            CommonToken nlToken = new CommonToken(this.curToken); // save the current NEWLINE token
            final boolean isLookingAhead = this.ffgToken.getType() == PythonLexer.WS;
            if (isLookingAhead) {
                this.setCurrentAndFollowingTokens(); // set the next two tokens
            }

            switch (this.ffgToken.getType()) {
                case PythonLexer.NEWLINE:      // We're before a blank line
                case PythonLexer.COMMENT:      // We're before a comment
                case PythonLexer.TYPE_COMMENT: // We're before a type comment
                    this.hideAndAddPendingToken(nlToken);
                    if (isLookingAhead) {
                        this.addPendingToken(this.curToken);  // WS token
                    }
                    break;
                default:
                    this.addPendingToken(nlToken);
                    if (isLookingAhead) { // We're on whitespace(s) followed by a statement
                        final int indentationLength = this.ffgToken.getType() == Token.EOF ?
                                                      0 :
                                                      this.getIndentationLength(this.curToken.getText());

                        if (indentationLength != this.INVALID_LENGTH) {
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

    private void insertIndentOrDedentToken(final int indentLength) {
        int prevIndentLength = this.indentLengthStack.peek();
        if (indentLength > prevIndentLength) {
            this.createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, null, this.ffgToken);
            this.indentLengthStack.push(indentLength);
        } else {
            while (indentLength < prevIndentLength) { // more than 1 DEDENT token may be inserted to the token stream
                this.indentLengthStack.pop();
                prevIndentLength = this.indentLengthStack.peek();
                if (indentLength <= prevIndentLength) {
                    this.createAndAddPendingToken(PythonLexer.DEDENT, Token.DEFAULT_CHANNEL, null, this.ffgToken);
                } else {
                    this.reportError("inconsistent dedent");
                }
            }
        }
    }

    private void handleSTRINGtoken() { // remove the \<newline> escape sequences from the string literal
        final String line_joinFreeStringLiteral = this.curToken.getText().replaceAll("\\\\\\r?\\n", "");
        if (this.curToken.getText().length() == line_joinFreeStringLiteral.length()) {
            this.addPendingToken(this.curToken);
        } else {
            CommonToken originalSTRINGtoken = new CommonToken(this.curToken); // backup the original token
            this.curToken.setText(line_joinFreeStringLiteral);
            this.addPendingToken(this.curToken);                  // add the modified token with inline string literal
            this.hideAndAddPendingToken(originalSTRINGtoken); // add the original token to the hidden channel
            // this inserted hidden token allows to restore the original string literal with the \<newline> escape sequences
        }
    }

    private void handleFSTRING_MIDDLE_token() { // replace the double braces '{{' or '}}' to single braces and hide the second braces
        String fsMid = this.curToken.getText();
        fsMid = fsMid.replaceAll("\\{\\{", "{_").replaceAll("}}", "}_"); // replace: {{ --> {_  and   }} --> }_
        String[] arrOfStr = fsMid.split("(?<=[{}])_"); // split by {_  or  }_
        for (String s : arrOfStr) {
            if (!s.isEmpty()) {
                this.createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, s, this.ffgToken);
                String lastCharacter = s.substring(s.length() - 1);
                if ("{}".contains(lastCharacter)) {
                    this.createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.HIDDEN_CHANNEL, lastCharacter, this.ffgToken);
                    // this inserted hidden token allows to restore the original f-string literal with the double braces
                }
            }
        }
    }

    private void handleFStringLexerModes() { // https://peps.python.org/pep-0498/#specification
        if (!this._modeStack.isEmpty()) {
            switch (this.curToken.getType()) {
                case PythonLexer.LBRACE:
                    this.pushMode(PythonLexer.DEFAULT_MODE);
                    this.paren_or_bracket_openedStack.push(0);
                    break;
                case PythonLexer.LPAR:
                case PythonLexer.LSQB:
                    // https://peps.python.org/pep-0498/#lambdas-inside-expressions
                    this.paren_or_bracket_openedStack.push(this.paren_or_bracket_openedStack.pop() + 1); // increment the last element
                    break;
                case PythonLexer.RPAR:
                case PythonLexer.RSQB:
                    this.paren_or_bracket_openedStack.push(this.paren_or_bracket_openedStack.pop() - 1); // decrement the last element
                    break;
                case PythonLexer.COLON: // colon can only come from DEFAULT_MODE
                    if (this.paren_or_bracket_openedStack.peek() == 0) {
                        switch (_modeStack.peek()) { // check the previous lexer mode (the current is DEFAULT_MODE)
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
                            this.paren_or_bracket_openedStack.pop();
                            break;
                        default:
                            this.reportLexerError("f-string: single '}' is not allowed");
                    }
                    break;
            }
        }
    }

    private void handleFORMAT_SPECIFICATION_MODE() {
        if (!this._modeStack.isEmpty() &&
            this.ffgToken.getType() == PythonLexer.RBRACE) {

            switch (this.curToken.getType()) {
                case PythonLexer.COLON:
                case PythonLexer.RBRACE:
                    // insert an empty FSTRING_MIDDLE token instead of the missing format specification
                    this.createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, "", this.ffgToken);
                    break;
            }
        }
    }

    private void insertTrailingTokens() {
        switch (this.lastPendingTokenTypeFromDefaultChannel) {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default:
                // insert an extra trailing NEWLINE token that serves as the end of the last statement
                this.createAndAddPendingToken(PythonLexer.NEWLINE, Token.DEFAULT_CHANNEL, null, this.ffgToken); // ffgToken is EOF
        }
        this.insertIndentOrDedentToken(0); // Now insert as much trailing DEDENT tokens as needed
    }

    private void handleEOFtoken() {
        if (this.lastPendingTokenTypeFromDefaultChannel > 0) {
            // there was statement in the input (leading NEWLINE tokens are hidden)
            this.insertTrailingTokens();
        }
        this.addPendingToken(this.curToken);
    }

    private void hideAndAddPendingToken(CommonToken cToken) {
        cToken.setChannel(Token.HIDDEN_CHANNEL);
        this.addPendingToken(cToken);
    }

    private void createAndAddPendingToken(final int type, final int channel, final String text, Token baseToken) {
        CommonToken cToken = new CommonToken(baseToken);
        cToken.setType(type);
        cToken.setChannel(channel);
        cToken.setStopIndex(baseToken.getStartIndex() - 1);
        cToken.setText(text == null
                       ? "<" + this.getVocabulary().getSymbolicName(type) + ">"
                       : text);

        this.addPendingToken(cToken);
    }

    private void addPendingToken(final Token token) {
        // save the last pending token type because the pendingTokens linked list can be empty by the nextToken()
        this.previousPendingTokenType = token.getType();
        if (token.getChannel() == Token.DEFAULT_CHANNEL) {
            this.lastPendingTokenTypeFromDefaultChannel = this.previousPendingTokenType;
        }
        this.pendingTokens.addLast(token);
    }

    private int getIndentationLength(final String textWS) { // the textWS may contain spaces, tabs or form feeds
        final int TAB_LENGTH = 8; // the standard number of spaces to replace a tab to spaces
        int length = 0;
        for (char ch : textWS.toCharArray()) {
            switch (ch) {
                case ' ':
                    this.wasSpaceIndentation = true;
                    length += 1;
                    break;
                case '\t':
                    this.wasTabIndentation = true;
                    length += TAB_LENGTH - (length % TAB_LENGTH);
                    break;
                case '\f': // form feed
                    length = 0;
                    break;
            }
        }

        if (this.wasTabIndentation && this.wasSpaceIndentation) {
            if (!(this.wasIndentationMixedWithSpacesAndTabs)) {
                this.wasIndentationMixedWithSpacesAndTabs = true;
                return this.INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    private void reportLexerError(final String errMsg) {
        this.getErrorListenerDispatch().syntaxError(this, this.curToken, this.curToken.getLine(), this.curToken.getCharPositionInLine(), " LEXER" + this.ERR_TXT + errMsg, null);
    }

    private void reportError(final String errMsg) {
        this.reportLexerError(errMsg);

        // the ERROR_TOKEN will raise an error in the parser
        this.createAndAddPendingToken(PythonLexer.ERROR_TOKEN, Token.DEFAULT_CHANNEL, this.ERR_TXT + errMsg, this.ffgToken);
    }

    @Override
    public void reset() {
        this.init();
        super.reset();
    }
}
