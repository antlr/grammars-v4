/*
BSD License
Copyright (c) 2020, Evgeniy Slobodkin
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

import org.antlr.v4.runtime.*;

import java.util.Stack;
import java.util.LinkedList;

public abstract class HaskellBaseLexer extends Lexer {

    public HaskellBaseLexer(CharStream input) {
        super(input);
    }

    public class Pair<L,R> {
        private final L left;
        private final R right;
        public Pair(L left, R right) {
            this.left = left;
            this.right = right;
        }

        public L first() { return left; }
        public R second() { return right; }

        @Override
        public int hashCode() { return left.hashCode() ^ right.hashCode(); }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof Pair)) return false;
            Pair pairo = (Pair) o;
            return this.left.equals(pairo.first()) &&
                this.right.equals(pairo.second());
        }
    }

    boolean pendingDent = true;

    // Current indent
    private int indentCount = 0;
    // A queue where extra tokens are pushed on
    private LinkedList<Token> tokenQueue = new LinkedList<>();
    // The stack that keeps key word and indent after that
    private Stack<Pair<String, Integer>> indentStack = new Stack<>();
    // Pointer keeps last indent token
    private Token initialIndentToken = null;
    private String  lastKeyWord = "";

    private boolean prevWasEndl = false;
    private boolean prevWasKeyWord = false;
    // Need, for example, in {}-block
    private boolean ignoreIndent = false;
    // Check moment, when you should calculate start indent
    // module ... where {now you should remember start indent}
    private boolean moduleStartIndent = false;
    private boolean wasModuleExport   = false;

    // Haskell saves indent before first() symbol as null indent
    private int startIndent = -1;
    // Count of "active" key words in this moment
    private int nestedLevel = 0;

    protected void processNEWLINEToken() {
        if (pendingDent) { setChannel(HIDDEN); }
        indentCount = 0;
        initialIndentToken = null;
    }

    protected void processTABToken() {
        setChannel(HIDDEN);
        if (pendingDent) {
            indentCount += 8*getText().length();
        }
    }

    protected void processWSToken() {
        setChannel(HIDDEN);
        if (pendingDent) {
            indentCount += getText().length();
        }
    }

    private int getSavedIndent() { return indentStack.empty() ? startIndent : indentStack.peek().second(); }

    private CommonToken
    createToken(int type, String text, Token next) {
        CommonToken token = new CommonToken(type, text);
        if (initialIndentToken != null) {
            token.setStartIndex(initialIndentToken.getStartIndex());
            token.setLine(initialIndentToken.getLine());
            token.setCharPositionInLine(initialIndentToken.getCharPositionInLine());
            token.setStopIndex(next.getStartIndex() - 1);
        }
        return token;
    }

    private void processINToken(Token next) {
        while (!indentStack.empty() && indentStack.peek().first() != "let") {
            tokenQueue.offer(createToken(HaskellLexer.SEMI, "SEMI", next));
            tokenQueue.offer(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
            nestedLevel--;
            indentStack.pop();
        }

        if (!indentStack.empty() && indentStack.peek().first().equals("let")) {
            tokenQueue.offer(createToken(HaskellLexer.SEMI, "SEMI", next));
            tokenQueue.offer(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
            nestedLevel--;
            indentStack.pop();
        }
    }

    private void processEOFToken(Token next) {
        indentCount = startIndent;
        if (!pendingDent) {
            initialIndentToken = next;
        }

        while (nestedLevel > indentStack.size()) {
            if (nestedLevel > 0)
                nestedLevel--;

            tokenQueue.offer(createToken(HaskellLexer.SEMI, "SEMI", next));
            tokenQueue.offer(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
        }

        while (indentCount < getSavedIndent()) {
            if (!indentStack.empty() && nestedLevel > 0) {
                indentStack.pop();
                nestedLevel--;
            }

            tokenQueue.offer(createToken(HaskellLexer.SEMI, "SEMI", next));
            tokenQueue.offer(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
        }

        if (indentCount == getSavedIndent()) {
            tokenQueue.offer(createToken(HaskellLexer.SEMI, "SEMI", next));
        }

        if (wasModuleExport) {
            tokenQueue.offer(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
        }

        startIndent = -1;
    }

    // Algorithm's description here:
    // https://www.haskell.org/onlinereport/haskell2010/haskellch10.html
    // https://en.wikibooks.org/wiki/Haskell/Indentation
    @Override
    public Token nextToken() {
        if (!tokenQueue.isEmpty()) {
            return tokenQueue.poll();
        }

        Token next = super.nextToken();

        if (startIndent == -1
            && next.getType() != HaskellLexer.NEWLINE
            && next.getType() !=  HaskellLexer.WS
            && next.getType() !=  HaskellLexer.TAB
            && next.getType() != HaskellLexer.OCURLY) {
            if (next.getType() ==  HaskellLexer.MODULE) {
                moduleStartIndent = true;
                wasModuleExport = true;
            } if (next.getType() !=  HaskellLexer.MODULE && !moduleStartIndent) {
                startIndent = next.getCharPositionInLine();
            } else if (lastKeyWord.equals("where") && moduleStartIndent) {
                lastKeyWord = "";
                prevWasKeyWord = false;
                nestedLevel = 0;
                moduleStartIndent = false;
                startIndent = next.getCharPositionInLine();
                tokenQueue.offer(createToken(HaskellLexer.VOCURLY, "VOCURLY", next));
                tokenQueue.offer(createToken(next.getType(), next.getText(), next));
                return tokenQueue.poll();
            }
        }

        if (next.getType() == HaskellLexer.OCURLY) {
            if (prevWasKeyWord) {
                nestedLevel--;
                prevWasKeyWord = false;
            }

            if (moduleStartIndent) {
                moduleStartIndent = false;
                // because will be  HaskellLexer.CCURLY in the end of file
                wasModuleExport = false;
            }

            ignoreIndent = true;
            prevWasEndl = false;
        }

        if (prevWasKeyWord && !prevWasEndl
            && !moduleStartIndent
            && next.getType() !=  HaskellLexer.WS
            && next.getType() != HaskellLexer.NEWLINE
            && next.getType() !=  HaskellLexer.TAB
            && next.getType() != HaskellLexer.OCURLY) {
            prevWasKeyWord = false;
            indentStack.push(new Pair<String, Integer>(lastKeyWord, next.getCharPositionInLine()));
            tokenQueue.offer(createToken(HaskellLexer.VOCURLY, "VOCURLY", next));
        }

        if (ignoreIndent
            && (next.getType() == HaskellLexer.WHERE
            || next.getType() == HaskellLexer.DO
            || next.getType() == HaskellLexer.LET
            || next.getType() ==  HaskellLexer.OF
            || next.getType() ==  HaskellLexer.CCURLY)
           ) {
            ignoreIndent = false;
        }

        if (pendingDent
            && prevWasKeyWord
            && !ignoreIndent
            && indentCount <= getSavedIndent()
            && next.getType() != HaskellLexer.NEWLINE
            && next.getType() !=  HaskellLexer.WS) {

            tokenQueue.offer(createToken(HaskellLexer.VOCURLY, "VOCURLY", next));
            prevWasKeyWord = false;
            prevWasEndl = true;
        }


        if (pendingDent && prevWasEndl
            && !ignoreIndent
            && indentCount <= getSavedIndent()
            && next.getType() != HaskellLexer.NEWLINE
            && next.getType() !=  HaskellLexer.WS
            && next.getType() != HaskellLexer.WHERE
            && next.getType() !=  HaskellLexer.IN
            && next.getType() != HaskellLexer.DO
            && next.getType() !=  HaskellLexer.OF
            && next.getType() !=  HaskellLexer.CCURLY
            && next.getType() != EOF) {

            while (nestedLevel > indentStack.size()) {
                if (nestedLevel > 0)
                    nestedLevel--;

                tokenQueue.offer(createToken(HaskellLexer.SEMI, "SEMI", next));
                tokenQueue.offer(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
            }

            while (indentCount < getSavedIndent()) {
                if (!indentStack.empty() && nestedLevel > 0) {
                    indentStack.pop();
                    nestedLevel--;
                }

                tokenQueue.offer(createToken(HaskellLexer.SEMI, "SEMI", next));
                tokenQueue.offer(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
            }

            if (indentCount == getSavedIndent()) {
                tokenQueue.offer(createToken(HaskellLexer.SEMI, "SEMI", next));
            }

            prevWasEndl = false;
            if (indentCount == startIndent) {
                pendingDent = false;
            }
        }


        if (pendingDent && prevWasKeyWord
            && !moduleStartIndent
            && !ignoreIndent
            && indentCount > getSavedIndent()
            && next.getType() != HaskellLexer.NEWLINE
            && next.getType() !=  HaskellLexer.WS
            && next.getType() != EOF) {

            prevWasKeyWord = false;

            if (prevWasEndl) {
                indentStack.push(new Pair<String, Integer>(lastKeyWord, indentCount));
                prevWasEndl = false;
            }

            tokenQueue.offer(createToken(HaskellLexer.VOCURLY, "VOCURLY", next));
        }

        if (pendingDent
            && initialIndentToken == null
            && HaskellLexer.NEWLINE != next.getType()) {
            initialIndentToken = next;
        }

        if (next != null && next.getType() == HaskellLexer.NEWLINE) {
            prevWasEndl = true;
        }

        if (next.getType() == HaskellLexer.WHERE
            || next.getType() == HaskellLexer.LET
            || next.getType() == HaskellLexer.DO
            || next.getType() ==  HaskellLexer.OF) {
            // if next will be HaskellLexer.OCURLY need to decrement nestedLevel
            nestedLevel++;
            prevWasKeyWord = true;
            prevWasEndl = false;
            lastKeyWord = next.getText();

            if (next.getType() == HaskellLexer.WHERE) {
                if (!indentStack.empty() && (indentStack.peek().first().equals("do"))) {
                    tokenQueue.offer(createToken(HaskellLexer.SEMI, "SEMI", next));
                    tokenQueue.offer(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
                    indentStack.pop();
                    nestedLevel--;
                }
            }
        }

        if (next != null && next.getType() == HaskellLexer.OCURLY) {
            prevWasKeyWord = false;
        }

        if (next == null || HIDDEN == next.getChannel() || HaskellLexer.NEWLINE == next.getType()) {
            return next;
        }

        if (next.getType() ==  HaskellLexer.IN) {
            processINToken(next);
        }

        if (next.getType() == EOF) {
            processEOFToken(next);
        }

        pendingDent = true;
        tokenQueue.offer(next);

        return tokenQueue.poll();
    }
}
