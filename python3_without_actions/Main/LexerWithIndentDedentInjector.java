/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2019 Robert Einhorn
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Project      : Python3 Indent/Dedent handler for ANTLR4 grammars
 *                https://github.com/
 * Developed by : Robert Einhorn, robert.einhorn.hu@gmail.com
 */

import org.antlr.v4.runtime.*;


// **********************************************************************************************************
// **** THE FOLLOWING IMPORT STATEMENT ALSO CAN BE USED IN ANTLR4 GRAMMAR FILE @lexer::header{} SECTION ****
// **********************************************************************************************************
import java.util.*;

public class LexerWithIndentDedentInjector extends Python3Lexer {
    public LexerWithIndentDedentInjector(CharStream input) {
        super(input);
    }


    // *************************************************************************************************
    // **** THE FOLLOWING SECTION ALSO CAN BE USED IN ANTLR4 GRAMMAR FILE @lexer::members{} SECTION ****
    // *************************************************************************************************

    // The stack that keeps track of the indentation length
    private final Stack<Integer> indentLengths = new Stack<>();

    // A queue where extra tokens are pushed on.
    private final Deque<Token> pendingTokens = new ArrayDeque<>();
    // An integer that stores the type of the last inserted pending token.
    private int lastInsertedTokenType;

    // The amount of opened braces, brackets and parenthesis.
    private int opened = 0;

    // Was space char(s) in the indentations?
    private boolean wasSpaceIndentation = false;
    // Was TAB char(s) in the indentations?
    private boolean wasTabIndentation = false;

    @Override
    public Token nextToken() {                                           //*** https://www.antlr.org/api/Java/org/antlr/v4/runtime/Lexer.html#nextToken()
        if (getCharIndex() == 0) {  // We're at the start of the input.
            indentLengths.push(0);  // initializing with the default indentation length 0
            insertLeadingTokens();
        }

        Token currentToken;
        while (true) {
            currentToken = super.nextToken();
            switch (currentToken.getType()) {                            //*** https://www.antlr.org/api/Java/org/antlr/v4/runtime/Lexer.html#getType()
                case OPEN_PAREN:
                case OPEN_BRACK:
                case OPEN_BRACE:
                    opened++;
                    pendingTokens.addLast(currentToken);  // insert the open parentheses/square bracket/curly brace token
                    break;
                case CLOSE_PAREN:
                case CLOSE_BRACK:
                case CLOSE_BRACE:
                    opened--;
                    pendingTokens.addLast(currentToken);  // insert the close parentheses/square bracket/curly brace token
                    break;
                case NEWLINE:
                    if (opened > 0) {                                    //*** https://docs.python.org/3/reference/lexical_analysis.html#implicit-line-joining
                        continue;  // We're inside an implicit line joining section, skip the NEWLINE token.
                    } else {
                        switch (_input.LA(1) /* next symbol */) {        //*** https://www.antlr.org/api/Java/org/antlr/v4/runtime/IntStream.html#LA(int)
                            case '\r':
                            case '\n':
                            case '\f':
                            case '#':                                      //*** https://docs.python.org/3/reference/lexical_analysis.html#blank-lines
                                continue;  // We're on a blank line or before a comment, skip the NEWLINE token.
                            default:
                                pendingTokens.addLast(currentToken); // insert the NEWLINE token
                                insertIndentDedentTokens();                //*** https://docs.python.org/3/reference/lexical_analysis.html#indentation
                        }
                    }
                    break;
                case EOF:
                    if (indentLengths.size() > 0) {
                        insertTrailingTokens();
                        pendingTokens.addLast(currentToken); // insert the EOF token
                        displayWarnings();
                        System.out.println("");
                    }
                    break;
                default:
                    pendingTokens.addLast(currentToken);
            }
            break; // exit from the "infinite" while loop
        }

        lastInsertedTokenType = pendingTokens.peekLast().getType();
        return pendingTokens.pollFirst();
    }

    private void insertLeadingTokens() {
        final String leadingSpacesAndTabs = getSpacesAndTabsFromTheCurrentPosition();
        if (!leadingSpacesAndTabs.isEmpty()) {  // the input starts with space(s) or tab(s)
            final int currentIndentLength = getIndentationLength(leadingSpacesAndTabs);
            Token token;
            token = getNewToken(
                    0,
                    -1,
                    leadingSpacesAndTabs,
                    NEWLINE,
                    1,
                    0);

            pendingTokens.addLast(token);
            token = getNewToken(
                    0,
                    -1,
                    "<inserted leading INDENT, " + getIndentationDescription(currentIndentLength) + ">",
                    Python3Parser.INDENT, // the generated name of the Python3Parser class is based on the current grammar name
                    1,
                    leadingSpacesAndTabs.length());

            pendingTokens.addLast(token);
            indentLengths.push(currentIndentLength);
            //System.err.println("ERROR:   line 1:" + leadingSpacesAndTabs.length() + " unexpected indent");
        }
    }

    private void insertIndentDedentTokens() {
        final int currentIndentLength = getIndentationLength(getText()); //*** https://www.antlr.org/api/Java/org/antlr/v4/runtime/TokenStream.html#getText()
        int previousIndentLength = indentLengths.peek();
        if (currentIndentLength > previousIndentLength) {
            insertToken("<inserted INDENT, " + getIndentationDescription(currentIndentLength) + ">", Python3Parser.INDENT); // the generated name of the Python3Parser class is based on the current grammar name
            indentLengths.push(currentIndentLength); // in order after the insertToken!
        } else if (currentIndentLength < previousIndentLength) {
            // More than 1 DEDENT tokens may be inserted.
            do {
                indentLengths.pop(); // in order before the insertToken!
                previousIndentLength = indentLengths.peek();
                if (currentIndentLength <= previousIndentLength) {
                    insertToken("<inserted DEDENT, " + getIndentationDescription(previousIndentLength) + ">", Python3Parser.DEDENT); // the generated name of the Python3Parser class is based on the current grammar name
                } else {
                    insertToken("!!! INCONSISTENT DEDENT !!! " + getIndentationDescription(currentIndentLength), Python3Parser.DEDENT); // the generated name of the Python3Parser class is based on the current grammar name
                    System.err.println("ERROR:   line " + getLine() + ":" + getCharPositionInLine() + " inconsistent dedent");
                }
            } while (currentIndentLength < previousIndentLength);
        }
    }

    private void insertTrailingTokens() {
        switch (lastInsertedTokenType) {
            case NEWLINE:
            case Python3Parser.DEDENT: // the generated name of the Python3Parser class is based on the current grammar name
                break; // no trailing NEWLINE token is needed
            default:
                // insert an extra trailing NEWLINE token that serves as the end of the statement
                insertToken("<inserted trailing NEWLINE>", NEWLINE);
        }

        // Now insert as much trailing DEDENT tokens as needed.
        int previousIndentLength = indentLengths.peek();
        while (previousIndentLength != 0) {  // indentLengths stack has been initialized with integer 0
            indentLengths.pop();  // in order before the insertToken!
            previousIndentLength = indentLengths.peek();
            insertToken("<inserted trailing DEDENT, " + getIndentationDescription(previousIndentLength) + ">", Python3Parser.DEDENT); // the generated name of the Python3Parser class is based on the current grammar name
        }
        indentLengths.pop();  // now indentLengths.size() is 0
    }

    private void displayWarnings() {
        if (wasSpaceIndentation && wasTabIndentation) {
            System.out.println("WARNING: mixture of spaces and tabs were used for indentation");
        }
    }

    private CommonToken getNewToken(int startIndex, int stopIndex, String text, int type, int line, int charPositionInLine) {
        //*** metadata settings *** - display format in grun: [@TOKENNUMBER,startIndex:stopIndex='text','<type>',line:charPositionInLine]
        final CommonToken token = new CommonToken(type, text);           //*** https://www.antlr.org/api/Java/org/antlr/v4/runtime/CommonToken.html
        token.setStartIndex(startIndex);                                 //*** https://www.antlr.org/api/Java/org/antlr/v4/runtime/CommonToken.html#setStartIndex(int)
        token.setStopIndex(stopIndex);                                   //*** https://www.antlr.org/api/Java/org/antlr/v4/runtime/CommonToken.html#setStopIndex(int)
        token.setLine(line);                                             //*** https://www.antlr.org/api/Java/org/antlr/v4/runtime/CommonToken.html#setLine(int)
        token.setCharPositionInLine(charPositionInLine);                 //*** https://www.antlr.org/api/Java/org/antlr/v4/runtime/CommonToken.html#setCharPositionInLine(int)
        return token;
    }

    private void insertToken(String text, int type) {
        final int startIndex = _tokenStartCharIndex + getText().length();//*** https://www.antlr.org/api/Java/org/antlr/v4/runtime/Lexer.html#_tokenStartCharIndex
        final CommonToken token =
                getNewToken(
                        startIndex,
                        startIndex - 1,
                        text,
                        type,
                        getLine(),                 //*** https://www.antlr.org/api/Java/org/antlr/v4/runtime/Lexer.html#getLine();
                        getCharPositionInLine());  //*** https://www.antlr.org/api/Java/org/antlr/v4/runtime/Lexer.html#getCharPositionInLine()

        pendingTokens.addLast(token);
    }

    private String getIndentationDescription(int lengthOfIndent) {
        return "length=" + lengthOfIndent + ", level=" + (indentLengths.size());
    }

    private String getSpacesAndTabsFromTheCurrentPosition() {
        StringBuilder spaces_and_tabs = new StringBuilder();
        int count = 1;
        char ch;

        while (true) {
            ch = (char) _input.LA(count);
            switch (ch) {
                case ' ': // space char
                case '\t':
                    spaces_and_tabs.append(ch);
                    count++;
                    continue;
                default:
                    return spaces_and_tabs.toString();
            }
        }
    }

    // Calculates the indentation of the provided spaces, taking the
    // following rules into account:
    //
    // "Tabs are replaced (from left to right) by one to eight spaces
    //  such that the total number of characters up to and including
    //  the replacement is a multiple of eight [...]"
    //
    //  -- https://docs.python.org/3.1/reference/lexical_analysis.html#indentation
    private int getIndentationLength(String textOfMatchedNEWLINE) {
        int count = 0;

        for (char ch : textOfMatchedNEWLINE.toCharArray()) {
            switch (ch) {
                case ' ':
                    // A normal space char.
                    wasSpaceIndentation = true;
                    count++;
                    break;
                case '\t':
                    wasTabIndentation = true;
                    count += 8 - (count % 8);
                    break;
            }
        }
        return count;
    }
}
