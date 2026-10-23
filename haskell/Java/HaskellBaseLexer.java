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

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.LinkedList;

public abstract class HaskellBaseLexer extends Lexer {

    public HaskellBaseLexer(CharStream input) {
        super(input);
    }

    protected void SetHidden()
    {
        setChannel(HIDDEN);
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
    private final Deque<Pair<String, Integer>> indentStack = new ArrayDeque<>();
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
    private boolean inPragmas         = false;

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

    private int getSavedIndent() { return indentStack.isEmpty() ? startIndent : indentStack.peek().second(); }

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
        while (!indentStack.isEmpty() && !indentStack.peek().first().equals("let")) {
            tokenQueue.offer(createToken(HaskellLexer.SEMI, "SEMI", next));
            tokenQueue.offer(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
            nestedLevel--;
            indentStack.pop();
        }

        if (!indentStack.isEmpty() && indentStack.peek().first().equals("let")) {
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
            if (!indentStack.isEmpty() && nestedLevel > 0) {
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
        int   type = next.getType();

        if (type == HaskellLexer.OpenPragmaBracket) {
            inPragmas = true;
        }

        if (startIndent == -1
            && type != HaskellLexer.NEWLINE
            && type !=  HaskellLexer.WS
            && type !=  HaskellLexer.TAB
            && type != HaskellLexer.OCURLY) {
            if (type ==  HaskellLexer.MODULE) {
                moduleStartIndent = true;
                wasModuleExport = true;
            } if (type !=  HaskellLexer.MODULE && !moduleStartIndent && !inPragmas) {
                startIndent = next.getCharPositionInLine();
            } else if (lastKeyWord.equals("where") && moduleStartIndent) {
                lastKeyWord = "";
                prevWasKeyWord = false;
                nestedLevel = 0;
                moduleStartIndent = false;
                prevWasEndl = false;
                startIndent = next.getCharPositionInLine();
                tokenQueue.offer(createToken(HaskellLexer.VOCURLY, "VOCURLY", next));
                tokenQueue.offer(createToken(type, next.getText(), next));

                return tokenQueue.poll();
            }
        }

        if (type == HaskellLexer.ClosePragmaBracket) {
            inPragmas = false;
        }

        if (type == HaskellLexer.OCURLY) {
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
            && type !=  HaskellLexer.WS
            && type != HaskellLexer.NEWLINE
            && type !=  HaskellLexer.TAB
            && type != HaskellLexer.OCURLY) {
            prevWasKeyWord = false;
            indentStack.push(new Pair<String, Integer>(lastKeyWord, next.getCharPositionInLine()));
            tokenQueue.offer(createToken(HaskellLexer.VOCURLY, "VOCURLY", next));
        }

        if (ignoreIndent
            && (type == HaskellLexer.WHERE
            ||  type == HaskellLexer.DO
            ||  type == HaskellLexer.MDO
            ||  type == HaskellLexer.LET
            ||  type == HaskellLexer.OF
            ||  type == HaskellLexer.LCASE
            ||  type == HaskellLexer.REC
            ||  type == HaskellLexer.CCURLY)
           ) {
            ignoreIndent = false;
        }

        if (pendingDent
            && prevWasKeyWord
            && !ignoreIndent
            && indentCount <= getSavedIndent()
            && type != HaskellLexer.NEWLINE
            && type !=  HaskellLexer.WS) {

            tokenQueue.offer(createToken(HaskellLexer.VOCURLY, "VOCURLY", next));
            prevWasKeyWord = false;
            prevWasEndl = true;
        }


        if (pendingDent && prevWasEndl
            && !ignoreIndent
            && indentCount <= getSavedIndent()
            && type != HaskellLexer.NEWLINE
            && type !=  HaskellLexer.WS
            && type != HaskellLexer.WHERE
            && type !=  HaskellLexer.IN
            && type != HaskellLexer.DO
            && type != HaskellLexer.MDO
            && type !=  HaskellLexer.OF
            && type != HaskellLexer.LCASE
            && type != HaskellLexer.REC
            && type !=  HaskellLexer.CCURLY
            && type != EOF) {

            while (nestedLevel > indentStack.size()) {
                if (nestedLevel > 0)
                    nestedLevel--;

                tokenQueue.offer(createToken(HaskellLexer.SEMI, "SEMI", next));
                tokenQueue.offer(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
            }

            while (indentCount < getSavedIndent()) {
                if (!indentStack.isEmpty() && nestedLevel > 0) {
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
            && type != HaskellLexer.NEWLINE
            && type !=  HaskellLexer.WS
            && type != EOF) {

            prevWasKeyWord = false;

            if (prevWasEndl) {
                indentStack.push(new Pair<String, Integer>(lastKeyWord, indentCount));
                prevWasEndl = false;
            }

            tokenQueue.offer(createToken(HaskellLexer.VOCURLY, "VOCURLY", next));
        }

        if (pendingDent
            && initialIndentToken == null
            && HaskellLexer.NEWLINE != type) {
            initialIndentToken = next;
        }

        if (next != null && type == HaskellLexer.NEWLINE) {
            prevWasEndl = true;
        }

        if (   type == HaskellLexer.WHERE
            || type == HaskellLexer.LET
            || type == HaskellLexer.DO
            || type == HaskellLexer.MDO
            || type == HaskellLexer.OF
            || type == HaskellLexer.LCASE
            || type == HaskellLexer.REC) {
            // if next will be HaskellLexer.OCURLY need to decrement nestedLevel
            nestedLevel++;
            prevWasKeyWord = true;
            prevWasEndl = false;
            lastKeyWord = next.getText();

            if (type == HaskellLexer.WHERE) {
                if (!indentStack.isEmpty()
                    && (indentStack.peek().first().equals("do") 
                    || indentStack.peek().first().equals("mdo"))) {
                    tokenQueue.offer(createToken(HaskellLexer.SEMI, "SEMI", next));
                    tokenQueue.offer(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
                    indentStack.pop();
                    nestedLevel--;
                }
            }
        }

        if (next != null && type == HaskellLexer.OCURLY) {
            prevWasKeyWord = false;
        }

        if (next == null || HIDDEN == next.getChannel() || HaskellLexer.NEWLINE == type) {
            return next;
        }

        if (type ==  HaskellLexer.IN) {
            processINToken(next);
        }

        if (type == EOF) {
            processEOFToken(next);
        }

        pendingDent = true;
        tokenQueue.offer(next);

        return tokenQueue.poll();
    }
}
