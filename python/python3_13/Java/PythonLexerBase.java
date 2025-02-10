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
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.antlr.v4.runtime.*;

public abstract class PythonLexerBase extends Lexer {
    // A stack that keeps track of the indentation lengths
    private Deque<Integer> indentLengthStack;
    // A list where tokens are waiting to be loaded into the token stream
    private Deque<Token> pendingTokens;

    // last pending token type
    private int previousPendingTokenType;
    private int lastPendingTokenTypeFromDefaultChannel;

    // The amount of opened parentheses, square brackets or curly braces
    private int opened;
    // The amount of opened parentheses and square brackets in the current lexer mode
    private Deque<Integer> paren_or_bracket_openedStack;
    // A stack that stores expression(s) between braces in fstring
    private Deque<String> braceExpressionStack;
    private String prevBraceExpression;

    // Instead of this._mode      (_mode is not implemented in each ANTLR4 runtime)
    private int curLexerMode;
    // Instead of this._modeStack (_modeStack is not implemented in each ANTLR4 runtime)
    private Deque<Integer> lexerModeStack;

    private boolean wasSpaceIndentation;
    private boolean wasTabIndentation;
    private boolean wasIndentationMixedWithSpacesAndTabs;

    private Token curToken; // current (under processing) token
    private Token ffgToken; // following (look ahead) token

    private final int INVALID_LENGTH = -1;
    private final String ERR_TXT = " ERROR: ";

    protected PythonLexerBase(CharStream input) {
        super(input);
        this.init();
    }

    @Override
    public Token nextToken() { // reading the input stream until a return EOF
        this.checkNextToken();
        return this.pendingTokens.pollFirst(); // add the queued token to the token stream
    }

    @Override
    public void reset() {
        this.init();
        super.reset();
    }

    private void init() {
        this.indentLengthStack = new ArrayDeque<>();
        this.pendingTokens = new ArrayDeque<>();
        this.previousPendingTokenType = 0;
        this.lastPendingTokenTypeFromDefaultChannel = 0;
        this.opened = 0;
        this.paren_or_bracket_openedStack = new ArrayDeque<>();
        this.braceExpressionStack = new ArrayDeque<>();
        this.prevBraceExpression = "";
        this.curLexerMode = 0;
        this.lexerModeStack = new ArrayDeque<>();
        this.wasSpaceIndentation = false;
        this.wasTabIndentation = false;
        this.wasIndentationMixedWithSpacesAndTabs = false;
        this.curToken = null;
        this.ffgToken = null;
    }

    private void checkNextToken() {
        if (this.previousPendingTokenType == Token.EOF)
            return;

        if (this.indentLengthStack.isEmpty()) { // We're at the first token
            this.insertENCODINGtoken();
            this.setCurrentAndFollowingTokens();
            this.handleStartOfInput();
        } else {
            this.setCurrentAndFollowingTokens();
        }

        switch (this.curToken.getType()) {
            case PythonLexer.NEWLINE:
                this.handleNEWLINEtoken();
                break;
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
            case PythonLexer.FSTRING_MIDDLE:
                this.handleFSTRING_MIDDLEtokenWithDoubleBrace(); // does not affect the opened field
                this.addPendingToken(this.curToken);
                break;
            case PythonLexer.COLONEQUAL:
                this.handleCOLONEQUALtokenInFString();
                break;
            case PythonLexer.ERRORTOKEN:
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

    private void setCurrentAndFollowingTokens() {
        this.curToken = this.ffgToken == null ?
                        super.nextToken() :
                        this.ffgToken;

        this.checkCurToken(); // ffgToken cannot be used in this method and its sub methods (ffgToken is not yet set)!

        this.ffgToken = this.curToken.getType() == Token.EOF ?
                        this.curToken :
                        super.nextToken();
    }

    private void insertENCODINGtoken() { // https://peps.python.org/pep-0263/
        StringBuilder lineBuilder = new StringBuilder();
        String encodingName = "";
        int lineCount = 0;
        final Pattern ws_commentPattern = Pattern.compile("^[ \\t\\f]*(#.*)?$");
        final CharStream charStream = this.getInputStream();
        final int size = charStream.size();

        charStream.seek(0);
        for (int i = 0; i < size; i++) {
            char c = (char) charStream.LA(i + 1);
            lineBuilder.append(c);

            if (c == '\n' || i == size - 1) {
                String line = lineBuilder.toString().replace("\r", "").replace("\n", "");
                if (ws_commentPattern.matcher(line).find()) { // WS* + COMMENT? found
                    encodingName = getEncodingName(line);
                    if (!encodingName.isEmpty()) {
                        break; // encoding found
                    }
                } else {
                    break; // statement or backslash found (line is not empty, not whitespace(s), not comment)
                }

                lineCount++;
                if (lineCount >= 2) {
                    break; // check only the first two lines
                }
                lineBuilder = new StringBuilder();
            }
        }

        if (encodingName.isEmpty()) {
            encodingName = "utf-8"; // default Python source code encoding
        }

        final CommonToken encodingToken = new CommonToken(PythonLexer.ENCODING, encodingName);
        encodingToken.setChannel(Token.HIDDEN_CHANNEL);
        this.addPendingToken(encodingToken);
    }

    private String getEncodingName(final String commentText) { // https://peps.python.org/pep-0263/#defining-the-encoding
        final Pattern encodingCommentPattern = Pattern.compile("^[ \\t\\f]*#.*?coding[:=][ \\t]*([-_.a-zA-Z0-9]+)");
        final Matcher matcher = encodingCommentPattern.matcher(commentText);
        return matcher.find() ? matcher.group(1) : "";
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
        }
        // continue the processing of the EOF token with checkNextToken()
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
        if (!this.lexerModeStack.isEmpty()) { // for multi line fstring literals
            this.addPendingToken(this.curToken);
        } else if (this.opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
            this.hideAndAddPendingToken(this.curToken);
        } else {
            final Token nlToken = new CommonToken(this.curToken); // save the current NEWLINE token
            final boolean isLookingAhead = this.ffgToken.getType() == PythonLexer.WS;
            if (isLookingAhead) {
                this.setCurrentAndFollowingTokens(); // set the next two tokens
            }

            switch (this.ffgToken.getType()) {
                case PythonLexer.NEWLINE: // We're before a blank line
                case PythonLexer.COMMENT: // We're before a comment
                    this.hideAndAddPendingToken(nlToken);
                    if (isLookingAhead) {
                        this.addPendingToken(this.curToken); // WS token
                    }
                    break;
                default:
                    this.addPendingToken(nlToken);
                    if (isLookingAhead) { // We're on a whitespace(s) followed by a statement
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

    private void checkCurToken() {
        switch (this.curToken.getType()) {
            case PythonLexer.FSTRING_START:
                this.setLexerModeByFSTRING_STARTtoken();
                return;
            case PythonLexer.FSTRING_MIDDLE:
                this.handleFSTRING_MIDDLEtokenWithQuoteAndLBrace(); // affect the opened field
                if (this.curToken.getType() == PythonLexer.FSTRING_MIDDLE)
                    return; // No curToken exchange happened
                break;
            case PythonLexer.FSTRING_END:
                this.popLexerMode();
                return;
            default:
                if (this.lexerModeStack.isEmpty())
                    return; // Not in fstring mode
        }

        switch (this.curToken.getType()) { // the following tokens can only come from default mode (after an LBRACE in fstring)
            case PythonLexer.NEWLINE:
                // append the current brace expression with the current newline
                this.appendToBraceExpression(this.curToken.getText());
                final CommonToken ctkn = new CommonToken(this.curToken);
                ctkn.setChannel(Token.HIDDEN_CHANNEL);
                this.curToken = ctkn;
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
                this.appendToBraceExpression(this.curToken.getText());
                // https://peps.python.org/pep-0498/#lambdas-inside-expressions
                this.incrementBraceStack();
                break;
            case PythonLexer.RPAR:
            case PythonLexer.RSQB:
                // append the current brace expression with a ")" or a "]"
                this.appendToBraceExpression(this.curToken.getText());
                this.decrementBraceStack();
                break;
            case PythonLexer.COLON:
            case PythonLexer.COLONEQUAL:
                // append the current brace expression with a ":" or a ":="
                this.appendToBraceExpression(this.curToken.getText());
                this.setLexerModeByCOLONorCOLONEQUALtoken();
                break;
            case PythonLexer.RBRACE:
                this.setLexerModeAfterRBRACEtoken();
                break;
            default:
                // append the current brace expression with the current token text
                this.appendToBraceExpression(this.curToken.getText());
        }
    }

    private void appendToBraceExpression(String text) {
        this.braceExpressionStack.push(this.braceExpressionStack.pop() + text);
    }

    private void incrementBraceStack() { // increment the last element (peek() + 1)
        this.paren_or_bracket_openedStack.push(this.paren_or_bracket_openedStack.pop() + 1);
    }

    private void decrementBraceStack() { // decrement the last element (peek() - 1)
        this.paren_or_bracket_openedStack.push(this.paren_or_bracket_openedStack.pop() - 1);
    }

    private void setLexerModeAfterRBRACEtoken() {
        switch (this.curLexerMode) {
            case Lexer.DEFAULT_MODE:
                this.popLexerMode();
                this.popByBRACE();
                break;
            case PythonLexer.SQ1__FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ1R_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1__FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1R_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3__FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3R_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3__FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3R_FORMAT_SPECIFICATION_MODE:
                this.popLexerMode();
                this.popLexerMode();
                this.popByBRACE();
                break;
            default:
                this.reportLexerError("f-string: single '}' is not allowed");
        }
    }

    private void setLexerModeByFSTRING_STARTtoken() {
        final String text = this.curToken.getText().toLowerCase();
        Map<String, Integer> modeMap = new HashMap<>();
        modeMap.put("f'", PythonLexer.SQ1__FSTRING_MODE);
        modeMap.put("rf'", PythonLexer.SQ1R_FSTRING_MODE);
        modeMap.put("fr'", PythonLexer.SQ1R_FSTRING_MODE);
        modeMap.put("f\"", PythonLexer.DQ1__FSTRING_MODE);
        modeMap.put("rf\"", PythonLexer.DQ1R_FSTRING_MODE);
        modeMap.put("fr\"", PythonLexer.DQ1R_FSTRING_MODE);
        modeMap.put("f'''", PythonLexer.SQ3__FSTRING_MODE);
        modeMap.put("rf'''", PythonLexer.SQ3R_FSTRING_MODE);
        modeMap.put("fr'''", PythonLexer.SQ3R_FSTRING_MODE);
        modeMap.put("f\"\"\"", PythonLexer.DQ3__FSTRING_MODE);
        modeMap.put("rf\"\"\"", PythonLexer.DQ3R_FSTRING_MODE);
        modeMap.put("fr\"\"\"", PythonLexer.DQ3R_FSTRING_MODE);

        Integer mode = modeMap.get(text);
        if (mode != null) {
            this.pushLexerMode(mode);
        }
    }

    private void setLexerModeByCOLONorCOLONEQUALtoken() {
        if (this.paren_or_bracket_openedStack.peek() == 0) {
            // COLONEQUAL token will be replaced with a COLON token in checkNextToken()
            switch (this.lexerModeStack.peek()) { // check the previous lexer mode (the current is DEFAULT_MODE)
                case PythonLexer.SQ1__FSTRING_MODE:
                case PythonLexer.SQ1__FORMAT_SPECIFICATION_MODE:
                    this.pushLexerMode(PythonLexer.SQ1__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.SQ1R_FSTRING_MODE:
                case PythonLexer.SQ1R_FORMAT_SPECIFICATION_MODE:
                    this.pushLexerMode(PythonLexer.SQ1R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.DQ1__FSTRING_MODE:
                case PythonLexer.DQ1__FORMAT_SPECIFICATION_MODE:
                    this.pushLexerMode(PythonLexer.DQ1__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.DQ1R_FSTRING_MODE:
                case PythonLexer.DQ1R_FORMAT_SPECIFICATION_MODE:
                    this.pushLexerMode(PythonLexer.DQ1R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.SQ3__FSTRING_MODE:
                case PythonLexer.SQ3__FORMAT_SPECIFICATION_MODE:
                    this.pushLexerMode(PythonLexer.SQ3__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.SQ3R_FSTRING_MODE:
                case PythonLexer.SQ3R_FORMAT_SPECIFICATION_MODE:
                    this.pushLexerMode(PythonLexer.SQ3R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.DQ3__FSTRING_MODE:
                case PythonLexer.DQ3__FORMAT_SPECIFICATION_MODE:
                    this.pushLexerMode(PythonLexer.DQ3__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.DQ3R_FSTRING_MODE:
                case PythonLexer.DQ3R_FORMAT_SPECIFICATION_MODE:
                    this.pushLexerMode(PythonLexer.DQ3R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
            }
        }
    }

    private void popByBRACE() {
        this.paren_or_bracket_openedStack.pop();
        this.prevBraceExpression = this.braceExpressionStack.pop() + "}";
        if (!this.braceExpressionStack.isEmpty()) {
            // append the current brace expression with the previous brace expression
            this.braceExpressionStack.push(this.braceExpressionStack.pop() + this.prevBraceExpression);
        }

    }

    private void handleFSTRING_MIDDLEtokenWithDoubleBrace() {
        // replace the trailing double brace with a single brace and insert a hidden brace token
        switch (this.getLastTwoCharsOfTheCurTokenText()) {
            case "{{":
                this.trimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", Token.HIDDEN_CHANNEL);
                break;
            case "}}":
                this.trimLastCharAddPendingTokenSetCurToken(PythonLexer.RBRACE, "}", Token.HIDDEN_CHANNEL);
                break;
        }
    }

    private void handleFSTRING_MIDDLEtokenWithQuoteAndLBrace() {
        // replace the trailing     quote + left_brace with a quote     and insert an LBRACE token
        // replace the trailing backslash + left_brace with a backslash and insert an LBRACE token
        switch (this.getLastTwoCharsOfTheCurTokenText()) {
            case "\"{":
            case "'{":
            case "\\{":
                this.trimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", Token.DEFAULT_CHANNEL);
                break;
        }
    }

    private String getLastTwoCharsOfTheCurTokenText() {
        final String curTokenText = this.curToken.getText();
        return curTokenText.length() >= 2 ? curTokenText.substring(curTokenText.length() - 2) : curTokenText;
    }

    private void trimLastCharAddPendingTokenSetCurToken(final int type, final String text, final int channel) {
        // trim the last char and add the modified curToken to the pendingTokens stack
        final String curTokenText = this.curToken.getText();
        final String tokenTextWithoutLastChar = curTokenText.substring(0, curTokenText.length() - 1);
        final CommonToken ctkn = new CommonToken(this.curToken);
        ctkn.setText(tokenTextWithoutLastChar);
        ctkn.setStopIndex(ctkn.getStopIndex() - 1);
        this.addPendingToken(ctkn);

        this.createNewCurToken(type, text, channel); // set curToken
    }

    private void handleCOLONEQUALtokenInFString() {
        if (!this.lexerModeStack.isEmpty() &&
            this.paren_or_bracket_openedStack.peek() == 0) {

            // In fstring a colonequal (walrus operator) can only be used in parentheses
            // Not in parentheses, replace COLONEQUAL token with COLON as format specifier
            // and insert the equal symbol to the following FSTRING_MIDDLE token
            CommonToken ctkn = new CommonToken(this.curToken);
            ctkn.setType(PythonLexer.COLON);
            ctkn.setText(":");
            ctkn.setStopIndex(ctkn.getStartIndex());
            this.curToken = ctkn;
            if (this.ffgToken.getType() == PythonLexer.FSTRING_MIDDLE) {
                ctkn = new CommonToken(this.ffgToken);
                ctkn.setText("=" + ctkn.getText());
                ctkn.setStartIndex(ctkn.getStartIndex() - 1);
                ctkn.setCharPositionInLine(ctkn.getCharPositionInLine() - 1);
                this.ffgToken = ctkn;
            } else {
                this.addPendingToken(this.curToken);
                this.createNewCurToken(PythonLexer.FSTRING_MIDDLE, "=", Token.DEFAULT_CHANNEL);
            }
        }
        this.addPendingToken(this.curToken);
    }

    private void createNewCurToken(final int type, final String text, final int channel) {
        final CommonToken ctkn = new CommonToken(this.curToken);
        ctkn.setType(type);
        ctkn.setText(text);
        ctkn.setChannel(channel);
        ctkn.setCharPositionInLine(ctkn.getCharPositionInLine() + 1);
        ctkn.setStartIndex(ctkn.getStartIndex() + 1);
        ctkn.setStopIndex(ctkn.getStartIndex());
        this.curToken = ctkn;
    }

    private void pushLexerMode(final int mode) {
        this.pushMode(mode);
        this.lexerModeStack.push(this.curLexerMode);
        this.curLexerMode = mode;
    }

    private void popLexerMode() {
        this.popMode();
        this.curLexerMode = this.lexerModeStack.pop();
    }

    private void handleFORMAT_SPECIFICATION_MODE() {
        if (!this.lexerModeStack.isEmpty() &&
            this.ffgToken.getType() == PythonLexer.RBRACE) {

            // insert an empty FSTRING_MIDDLE token instead of the missing format specification
            switch (this.curToken.getType()) {
                case PythonLexer.COLON:
                    this.createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, "", this.ffgToken);
                    break;
                case PythonLexer.RBRACE:
                    // only if the previous brace expression is not a dictionary comprehension or set comprehension
                    if (!isDictionaryComprehensionOrSetComprehension(this.prevBraceExpression)) {
                        this.createAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, "", this.ffgToken);
                    }
                    break;
            }
        }
    }

    private boolean isDictionaryComprehensionOrSetComprehension(final String code) {
        final CharStream inputStream = CharStreams.fromString(code);
        final PythonLexer lexer = new PythonLexer(inputStream);
        final CommonTokenStream tokenStream = new CommonTokenStream(lexer);
        PythonParser parser = new PythonParser(tokenStream);

        // Disable error listeners to suppress console output
        lexer.removeErrorListeners();
        parser.removeErrorListeners();

        parser.dictcomp(); // Try parsing as dictionary comprehension
        if (parser.getNumberOfSyntaxErrors() == 0)
            return  true;

        parser = new PythonParser(tokenStream);
        tokenStream.seek(0);
        parser.removeErrorListeners();
        parser.setcomp(); // Try parsing as set comprehension
        return parser.getNumberOfSyntaxErrors() == 0;
    }

    private void insertTrailingTokens() {
        switch (this.lastPendingTokenTypeFromDefaultChannel) {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default: // insert an extra trailing NEWLINE token that serves as the end of the last statement
                this.createAndAddPendingToken(PythonLexer.NEWLINE, Token.DEFAULT_CHANNEL, null, this.ffgToken); // ffgToken is EOF
        }
        this.insertIndentOrDedentToken(0); // Now insert as much trailing DEDENT tokens as needed
    }

    private void handleEOFtoken() {
        if (this.lastPendingTokenTypeFromDefaultChannel > 0) {
            // there was a statement in the input (leading NEWLINE tokens are hidden)
            this.insertTrailingTokens();
        }
        this.addPendingToken(this.curToken);
    }

    private void hideAndAddPendingToken(final Token tkn) {
        final CommonToken ctkn = new CommonToken(tkn);
        ctkn.setChannel(Token.HIDDEN_CHANNEL);
        this.addPendingToken(ctkn);
    }

    private void createAndAddPendingToken(final int ttype, final int channel, final String text, final Token sampleToken) {
        final CommonToken ctkn = new CommonToken(sampleToken);
        ctkn.setType(ttype);
        ctkn.setChannel(channel);
        ctkn.setStopIndex(sampleToken.getStartIndex() - 1);
        ctkn.setText(text == null ?
                     "<" + this.getVocabulary().getDisplayName(ttype) + ">" :
                     text);

        this.addPendingToken(ctkn);
    }

    private void addPendingToken(final Token tkn) {
        // save the last pending token type because the pendingTokens list can be empty by the nextToken()
        this.previousPendingTokenType = tkn.getType();
        if (tkn.getChannel() == Token.DEFAULT_CHANNEL) {
            this.lastPendingTokenTypeFromDefaultChannel = this.previousPendingTokenType;
        }
        this.pendingTokens.addLast(tkn);
    }

    private int getIndentationLength(final String indentText) { // the indentText may contain spaces, tabs or form feeds
        final int TAB_LENGTH = 8; // the standard number of spaces to replace a tab with spaces
        int length = 0;
        for (char ch : indentText.toCharArray()) {
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
                length = this.INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    private void reportLexerError(final String errMsg) {
        this.getErrorListenerDispatch().syntaxError(this, this.curToken, this.curToken.getLine(), this.curToken.getCharPositionInLine(), " LEXER" + this.ERR_TXT + errMsg, null);
    }

    private void reportError(final String errMsg) {
        this.reportLexerError(errMsg);

        // the ERRORTOKEN will raise an error in the parser
        this.createAndAddPendingToken(PythonLexer.ERRORTOKEN, Token.DEFAULT_CHANNEL, this.ERR_TXT + errMsg, this.ffgToken);
    }
}
