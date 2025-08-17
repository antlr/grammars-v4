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
         this.pendingTokens = new LinkedList<>();
         this.previousPendingTokenType = 0;
         this.lastPendingTokenTypeFromDefaultChannel = 0;
         this.opened = 0;
         this.wasSpaceIndentation = false;
         this.wasTabIndentation = false;
         this.wasIndentationMixedWithSpacesAndTabs = false;
         this.curToken = null;
         this.ffgToken = null;
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
         }
     }
 
     private void setCurrentAndFollowingTokens() {
         this.curToken = this.ffgToken == null ?
                         super.nextToken() :
                         this.ffgToken;
 
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
         if (this.opened > 0) { // We're in an implicit line joining, ignore the current NEWLINE token
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
 
     private void hideAndAddPendingToken(final Token tkn) {
         CommonToken ctkn = new CommonToken(tkn);
         ctkn.setChannel(Token.HIDDEN_CHANNEL);
         this.addPendingToken(ctkn);
     }
 
     private void createAndAddPendingToken(final int ttype, final int channel, final String text, Token sampleToken) {
         CommonToken ctkn = new CommonToken(sampleToken);
         ctkn.setType(ttype);
         ctkn.setChannel(channel);
         ctkn.setStopIndex(sampleToken.getStartIndex() - 1);
         ctkn.setText(text == null
                      ? "<" + this.getVocabulary().getDisplayName(ttype) + ">"
                      : text);
 
         this.addPendingToken(ctkn);
     }
 
     private void addPendingToken(final Token tkn) {
         // save the last pending token type because the pendingTokens linked list can be empty by the nextToken()
         this.previousPendingTokenType = tkn.getType();
         if (tkn.getChannel() == Token.DEFAULT_CHANNEL) {
             this.lastPendingTokenTypeFromDefaultChannel = this.previousPendingTokenType;
         }
         this.pendingTokens.addLast(tkn);
     }
 
     private int getIndentationLength(final String indentText) { // the indentText may contain spaces, tabs or form feeds
         final int TAB_LENGTH = 8; // the standard number of spaces to replace a tab to spaces
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
 