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
 * Project      : Python Indent/Dedent handler for ANTLR4 grammars
 *
 * Developed by : Robert Einhorn
 */


// *** lexer grammar dependencies to use this class with other (older) ANTLR4 Python grammars
/*

lexer grammar PythonLexer;
options { superClass=PythonLexerBase; }
tokens { INDENT, DEDENT }

OPEN_PAREN   : '(';
OPEN_BRACK   : '[';
OPEN_BRACE   : '{';
CLOSE_PAREN  : ')';
CLOSE_BRACK  : ']';
CLOSE_BRACE  : '}';
TYPE_COMMENT : '#' WS? 'type:' WS? ~[\r\n\f]*;
NEWLINE      : OS_INDEPENDENT_NL;
COMMENT      : '#' ~[\r\n\f]* -> channel(HIDDEN);
WS           : [ \t]+         -> channel(HIDDEN);
LINE_JOINING : '\\' NEWLINE   -> channel(HIDDEN);

fragment OS_INDEPENDENT_NL : '\r'? '\n';

 */


import java.util.*;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.CommonToken;

public abstract class PythonLexerBase extends Lexer {
    protected PythonLexerBase(CharStream input) {
        super(input);
    }

    // A stack that keeps track of the indentation lengths
    private final Deque<Integer> _indentLengths = new ArrayDeque<>();
    // A linked list where tokens are waiting to be loaded into the token stream
    private final LinkedList<Token> _pendingTokens = new LinkedList<>();
    // last pending token types
    private int _previousPendingTokenType = 0;
    private int _lastPendingTokenTypeForDefaultChannel = 0;

    // The amount of opened parentheses, square brackets or curly braces
    private int _opened = 0;

    // Was there a space char in the indentations?
    private boolean _wasSpaceIndentation = false;
    // The last number of the line of the indentation that used tab char
    private int _lastLineOfTabbedIndentation = 0;

    private Token _curToken; // current (under processing) token
    private Token _ffgToken; // following (look ahead) token

    @Override
    public Token nextToken() { // reading of the input stream until a return EOF
        if (_input.size() == 0) {
            return new CommonToken(EOF, "<EOF>");
        } else {
            checkNextToken();
            return _pendingTokens.pollFirst(); // add the queued token to the token stream
        }
    }

    private void checkNextToken() {
        if (_previousPendingTokenType != EOF) {
            setCurrentAndFollowingTokens();
            handleStartOfInput();
            switch (_curToken.getType()) { // Java 8 switch
                case PythonLexer.OPEN_PAREN:
                case PythonLexer.OPEN_BRACK:
                case PythonLexer.OPEN_BRACE:
                    _opened++;
                    addPendingToken(_curToken);
                    break;
                case PythonLexer.CLOSE_PAREN:
                case PythonLexer.CLOSE_BRACK:
                case PythonLexer.CLOSE_BRACE:
                    _opened--;
                    addPendingToken(_curToken);
                    break;
                case PythonLexer.NEWLINE:
                    handleNEWLINEtoken();
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
        _curToken = _ffgToken == null ? super.nextToken() : _ffgToken;
        _ffgToken = _curToken.getType() == EOF ? _curToken : super.nextToken();
    }

    // initialize the _indentLengths stack
    // hide the leading NEWLINE token(s)
    // if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    // insert a leading INDENT token if necessary
    private void handleStartOfInput() {
        if (_indentLengths.size() == 0) { // We're at the first token
            // initialize the stack with a default 0 indentation length
            _indentLengths.push(0); // this will never be popped off
            while (_curToken.getType() != EOF) {
                if (_curToken.getChannel() == Lexer.DEFAULT_TOKEN_CHANNEL) {
                    if (_curToken.getType() == PythonLexer.NEWLINE) {
                        // all the NEWLINE tokens must be ignored before the first statement
                        hideAndAddPendingToken(_curToken);
                    } else { // We're at the first statement
                        insertLeadingIndentToken();
                        return; // continue the processing of the current token with checkNextToken()
                    }
                } else {
                    addPendingToken(_curToken); // it can be WS, LINE_JOINING or COMMENT token
                }
                setCurrentAndFollowingTokens();
            } // continue the processing of the EOF token with checkNextToken()
        }
    }

    private void insertLeadingIndentToken() {
        if (_previousPendingTokenType == PythonLexer.WS) { // there is an "indentation" before the first statement
            // insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
            createAndAddPendingToken(PythonLexer.INDENT, _curToken); // insert an INDENT token before the _curToken
        }
    }

    protected void handleNEWLINEtoken() {
        if (_opened > 0) { //*** https://docs.python.org/3/reference/lexical_analysis.html#implicit-line-joining
            hideAndAddPendingToken(_curToken); // We're in an implicit line joining, ignore the current NEWLINE token
        } else {
            Token nlToken = _curToken; // save the current NEWLINE token
            boolean isLookingAhead = _ffgToken.getType() == PythonLexer.WS;
            if (isLookingAhead) {
                setCurrentAndFollowingTokens(); // set the two next tokens
            }

            if (_ffgToken.getType() == PythonLexer.NEWLINE ||      // We're before a blank line
                _ffgToken.getType() == PythonLexer.COMMENT ||      // We're before a comment
                _ffgToken.getType() == PythonLexer.TYPE_COMMENT) { // We're before a type comment

                hideAndAddPendingToken(nlToken); // ignore the NEWLINE token
                if (isLookingAhead) {
                    addPendingToken(_curToken);  // add the current WS token to the token stream
                }
            } else if (isLookingAhead) { // We're on a whitespace(s) followed by a statement
                addPendingToken(nlToken);        // add the NEWLINE token to the token stream
                addPendingToken(_curToken);      // add the current WS token to the token stream
                int indentationLength = _ffgToken.getType() == EOF ? 0 : getIndentationLength(_curToken.getText());
                insertIndentOrDedentToken(indentationLength); // may insert INDENT token or DEDENT token(s)
            } else { // We're before a statement (there is no whitespace before the statement)
                addPendingToken(nlToken);        // add the current NEWLINE token to the token stream
                insertIndentOrDedentToken(0); // may insert DEDENT token(s)
            }
        }
    }

    private void insertIndentOrDedentToken(int curIndentLength) {
        //*** https://docs.python.org/3/reference/lexical_analysis.html#indentation
        int prevIndentLength = _indentLengths.peek(); // never has null value
        if (curIndentLength > prevIndentLength) {
            createAndAddPendingToken(PythonLexer.INDENT, _ffgToken); // insert an INDENT token before the _ffgToken
            _indentLengths.push(curIndentLength);
        } else {
            while (curIndentLength < prevIndentLength) { // more than 1 DEDENT token may be inserted to the token stream
                _indentLengths.pop();
                prevIndentLength = _indentLengths.peek(); // never has null value
                createAndAddPendingToken(PythonLexer.DEDENT, _ffgToken); // insert a DEDENT token before the _ffgToken
                if (curIndentLength > prevIndentLength) {
                    IndentationErrorListener.lexerError(" line " + _ffgToken.getLine()
                                                        + ": \t unindent does not match any outer indentation level");
                }
            }
        }
    }

    private void handleEOFtoken() {
        if (_lastPendingTokenTypeForDefaultChannel > 0) { // there was statement in the input (leading NEWLINE tokens are hidden)
            insertTrailingTokens();
            checkSpaceAndTabIndentation();
        }
        addPendingToken(_curToken); // add the current EOF token to the token stream
    }

    private void insertTrailingTokens() {
        switch (_lastPendingTokenTypeForDefaultChannel) { // Java 8 switch
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default:
                //insert an extra trailing NEWLINE token that serves as the end of the last statement
                createAndAddPendingToken(PythonLexer.NEWLINE, _ffgToken); // insert before the _ffgToken
        }
        insertIndentOrDedentToken(0); // Now insert as much trailing DEDENT token(s) as needed
    }

    private void hideAndAddPendingToken(Token token) {
        // create a hidden copy of the token and add it to the pending tokens
        createAndAddPendingToken(token.getStartIndex(), token.getStopIndex(), token.getText()
        , token.getType(), Lexer.HIDDEN, token.getLine(), token.getCharPositionInLine());
    }

    private void createAndAddPendingToken(int type, Token followingToken) {
        createAndAddPendingToken(followingToken.getStartIndex(), followingToken.getStartIndex() - 1
        , "<" + getVocabulary().getDisplayName(type) + ">", type
        , Lexer.DEFAULT_TOKEN_CHANNEL
        , followingToken.getLine(), followingToken.getCharPositionInLine());
    }

    private void createAndAddPendingToken(int startIndex, int stopIndex, String text, int type, int channel
    , int line, int charPositionInLine) {

        CommonToken token = new CommonToken(_tokenFactorySourcePair, type, channel, startIndex, stopIndex);
        token.setText(text);
        token.setLine(line);
        token.setCharPositionInLine(charPositionInLine);
        addPendingToken(token);
    }

    private void addPendingToken(Token token) {
        // save the last pending token type because the _pendingTokens linked list can be empty by the nextToken()
        _previousPendingTokenType = token.getType();
        if (token.getChannel() == Lexer.DEFAULT_TOKEN_CHANNEL) {
            _lastPendingTokenTypeForDefaultChannel = _previousPendingTokenType;
        }
        _pendingTokens.addLast(token); // the token will be added to the token stream
    }

    // Calculates the indentation of the provided spaces, taking the
    // following rules into account:
    //
    // "Tabs are replaced (from left to right) by one to eight spaces
    //  such that the total number of characters up to and including
    //  the replacement is a multiple of eight [...]"
    //
    //  -- https://docs.python.org/3/reference/lexical_analysis.html#indentation
    private int getIndentationLength(String whiteSpaces) {
        final int TAB_LENGTH = 8; // the standard number of spaces to replace a tab to spaces
        int length = 0;
        for (int i = 0; i < whiteSpaces.length(); i++) {
            switch (whiteSpaces.charAt(i)) { // Java 8 switch
                case ' ': // A normal space char
                    _wasSpaceIndentation = true;
                    length += 1;
                    break;
                case '\t':
                    _lastLineOfTabbedIndentation = _curToken.getLine();
                    length += TAB_LENGTH - (length % TAB_LENGTH);
                    break;
            }
        }
        return length;
    }

    private void checkSpaceAndTabIndentation() {
        if (_wasSpaceIndentation && _lastLineOfTabbedIndentation > 0) {
            IndentationErrorListener.lexerError(" line " + _lastLineOfTabbedIndentation
                                                + ":\t inconsistent use of tabs and spaces in indentation");
        }
    }
}
