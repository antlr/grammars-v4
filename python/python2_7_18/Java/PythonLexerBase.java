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
    private LinkedList<Integer> _indentLengths = new LinkedList<>();
    // A linked list where tokens are waiting to be loaded into the token stream
    private LinkedList<Token> _pendingTokens = new LinkedList<>();

    // last pending token types
    private int _previousPendingTokenType = 0;
    private int _lastPendingTokenTypeForDefaultChannel = 0;

    // The amount of opened parentheses, square brackets or curly braces
    private int _opened = 0;

    private boolean _wasSpaceIndentation = false;
    private boolean _wasTabIndentation = false;
    private boolean _wasIndentationMixedWithSpacesAndTabs = false;
    private final int _INVALID_LENGTH = -1;

    private CommonToken _curToken; // current (under processing) token
    private Token _ffgToken; // following (look ahead) token

    private final String _ERR_TXT = " ERROR: ";

    protected PythonLexerBase(CharStream input) {
        super(input);
    }

    @Override
    public Token nextToken() { // reading the input stream until a return EOF
        checkNextToken();
        return _pendingTokens.pollFirst(); // add the queued token to the token stream
    }

    private void checkNextToken() {
        if (_previousPendingTokenType != EOF) {
            setCurrentAndFollowingTokens();
            if (_indentLengths.size() == 0) { // We're at the first token
                handleStartOfInput();
            }

            switch (_curToken.getType()) {
                case PythonLexer.LPAR:
                case PythonLexer.LSQB:
                case PythonLexer.LBRACE:
                    _opened++;
                    addPendingToken(_curToken);
                    break;
                case PythonLexer.RPAR:
                case PythonLexer.RSQB:
                case PythonLexer.RBRACE:
                    _opened--;
                    addPendingToken(_curToken);
                    break;
                case PythonLexer.NEWLINE:
                    handleNEWLINEtoken();
                    break;
                case PythonLexer.STRING:
                    handleSTRINGtoken();
                    break;
                case PythonLexer.ERROR_TOKEN:
                    reportLexerError("token recognition error at: '" + _curToken.getText() + "'");
                    addPendingToken(_curToken);
                    break;
                case EOF:
                    handleEOFtoken();
                    break;
                default:
                    addPendingToken(_curToken);
            }
        }
    }

    private void setCurrentAndFollowingTokens() {
        _curToken = _ffgToken == null ?
                    new CommonToken(super.nextToken()) :
                    new CommonToken(_ffgToken);

        _ffgToken = _curToken.getType() == EOF ?
                    _curToken :
                    super.nextToken();
    }

    // initialize the _indentLengths stack
    // hide the leading NEWLINE token(s)
    // if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    // insert a leading INDENT token if necessary
    private void handleStartOfInput() {
        // initialize the stack with a default 0 indentation length
        _indentLengths.addLast(0); // this will never be popped off
        while (_curToken.getType() != EOF) {
            if (_curToken.getChannel() == Token.DEFAULT_CHANNEL) {
                if (_curToken.getType() == PythonLexer.NEWLINE) {
                    // all the NEWLINE tokens must be ignored before the first statement
                    hideAndAddPendingToken(_curToken);
                } else { // We're at the first statement
                    insertLeadingIndentToken();
                    return; // continue the processing of the current token with checkNextToken()
                }
            } else {
                addPendingToken(_curToken); // it can be WS, EXPLICIT_LINE_JOINING or COMMENT token
            }
            setCurrentAndFollowingTokens();
        } // continue the processing of the EOF token with checkNextToken()
    }

    private void insertLeadingIndentToken() {
        if (_previousPendingTokenType == PythonLexer.WS) {
            Token prevToken = _pendingTokens.peekLast(); // WS token
            if (getIndentationLength(prevToken.getText()) != 0) { // there is an "indentation" before the first statement
                final String errMsg = "first statement indented";
                reportLexerError(errMsg);
                // insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
                createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, _ERR_TXT + errMsg, _curToken);
            }
        }
    }

    private void handleNEWLINEtoken() {
        if (_opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
            hideAndAddPendingToken(_curToken);
        } else {
            CommonToken nlToken = _curToken; // save the current NEWLINE token
            final boolean isLookingAhead = _ffgToken.getType() == PythonLexer.WS;
            if (isLookingAhead) {
                setCurrentAndFollowingTokens(); // set the two next tokens
            }

            switch (_ffgToken.getType()) {
                case PythonLexer.NEWLINE:      // We're before a blank line
                case PythonLexer.COMMENT:      // We're before a comment
                    hideAndAddPendingToken(nlToken);
                    if (isLookingAhead) {
                        addPendingToken(_curToken);  // WS token
                    }
                    break;
                default:
                    addPendingToken(nlToken);
                    if (isLookingAhead) { // We're on a whitespace(s) followed by a statement
                        final int indentationLength = _ffgToken.getType() == EOF ?
                                                      0 :
                                                      getIndentationLength(_curToken.getText());

                        if (indentationLength != _INVALID_LENGTH) {
                            addPendingToken(_curToken); // WS token
                            insertIndentOrDedentToken(indentationLength); // may insert INDENT token or DEDENT token(s)
                        } else {
                            reportError("inconsistent use of tabs and spaces in indentation");
                        }
                    } else { // We're at a newline followed by a statement (there is no whitespace before the statement)
                        insertIndentOrDedentToken(0); // may insert DEDENT token(s)
                    }
            }
        }
    }

    private void insertIndentOrDedentToken(final int curIndentLength) {
        int prevIndentLength = _indentLengths.peekLast();
        if (curIndentLength > prevIndentLength) {
            createAndAddPendingToken(PythonLexer.INDENT, Token.DEFAULT_CHANNEL, null, _ffgToken);
            _indentLengths.addLast(curIndentLength);
        } else {
            while (curIndentLength < prevIndentLength) { // more than 1 DEDENT token may be inserted to the token stream
                _indentLengths.removeLast();
                prevIndentLength = _indentLengths.peekLast();
                if (curIndentLength <= prevIndentLength) {
                    createAndAddPendingToken(PythonLexer.DEDENT, Token.DEFAULT_CHANNEL, null, _ffgToken);
                } else {
                    reportError("inconsistent dedent");
                }
            }
        }
    }

    private void handleSTRINGtoken() { // remove the \<newline> escape sequences from the string literal
        final String line_joinFreeStringLiteral = _curToken.getText().replaceAll("\\\\\\r?\\n", "");
        if (_curToken.getText().length() == line_joinFreeStringLiteral.length()) {
            addPendingToken(_curToken);
        } else {
            CommonToken originalSTRINGtoken = new CommonToken(_curToken); // backup the original token
            _curToken.setText(line_joinFreeStringLiteral);
            addPendingToken(_curToken);                  // add the modified token with inline string literal
            hideAndAddPendingToken(originalSTRINGtoken); // add the original token to the hidden channel
            // this inserted hidden token allows to restore the original string literal with the \<newline> escape sequences
        }
    }

    private void insertTrailingTokens() {
        switch (_lastPendingTokenTypeForDefaultChannel) {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default:
                // insert an extra trailing NEWLINE token that serves as the end of the last statement
                createAndAddPendingToken(PythonLexer.NEWLINE, Token.DEFAULT_CHANNEL, null, _ffgToken); // _ffgToken is EOF
        }
        insertIndentOrDedentToken(0); // Now insert as much trailing DEDENT tokens as needed
    }

    private void handleEOFtoken() {
        if (_lastPendingTokenTypeForDefaultChannel > 0) {
            // there was statement in the input (leading NEWLINE tokens are hidden)
            insertTrailingTokens();
        }
        addPendingToken(_curToken);
    }

    private void hideAndAddPendingToken(CommonToken token) {
        token.setChannel(Token.HIDDEN_CHANNEL);
        addPendingToken(token);
    }

    private void createAndAddPendingToken(final int type, final int channel, final String text, Token baseToken) {
        CommonToken token = new CommonToken(baseToken);
        token.setType(type);
        token.setChannel(channel);
        token.setStopIndex(baseToken.getStartIndex() - 1);
        token.setText(text == null
                      ? "<" + getVocabulary().getSymbolicName(type) + ">"
                      : text);

        addPendingToken(token);
    }

    private void addPendingToken(Token token) {
        // save the last pending token type because the _pendingTokens linked list can be empty by the nextToken()
        _previousPendingTokenType = token.getType();
        if (token.getChannel() == Token.DEFAULT_CHANNEL) {
            _lastPendingTokenTypeForDefaultChannel = _previousPendingTokenType;
        }
        _pendingTokens.addLast(token);
    }

    private int getIndentationLength(final String textWS) { // the textWS may contain spaces, tabs or formfeeds
        final int TAB_LENGTH = 8; // the standard number of spaces to replace a tab to spaces
        int length = 0;
        for (char ch : textWS.toCharArray()) {
            switch (ch) {
                case ' ':
                    _wasSpaceIndentation = true;
                    length += 1;
                    break;
                case '\t':
                    _wasTabIndentation = true;
                    length += TAB_LENGTH - (length % TAB_LENGTH);
                    break;
                case '\f': // formfeed
                    length = 0;
                    break;
            }
        }

        if (_wasTabIndentation && _wasSpaceIndentation) {
            if (!_wasIndentationMixedWithSpacesAndTabs) {
                _wasIndentationMixedWithSpacesAndTabs = true;
                return _INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    private void reportLexerError(final String errMsg) {
        getErrorListenerDispatch().syntaxError(this, _curToken, _curToken.getLine(), _curToken.getCharPositionInLine(), _ERR_TXT + errMsg, null);
    }

    private void reportError(final String errMsg) {
        reportLexerError(errMsg);

        // the ERROR_TOKEN will raise an error in the parser
        createAndAddPendingToken(PythonLexer.ERROR_TOKEN, Token.DEFAULT_CHANNEL, _ERR_TXT + errMsg, _ffgToken);
    }
}
