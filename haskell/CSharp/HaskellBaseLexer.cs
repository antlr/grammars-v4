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

using Antlr4.Runtime;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public abstract class HaskellBaseLexer : Lexer
{
    public HaskellBaseLexer(ICharStream input)
        : base(input)
    {
    }

    protected HaskellBaseLexer(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    protected void SetHidden()
    {
        this.Channel = Antlr4.Runtime.TokenConstants.HiddenChannel;
    }

    public class Pair<L,R>
    {
        private L left;
        private R right;
        public Pair(L left, R right) {
            this.left = left;
            this.right = right;
        }

        public L first() { return left; }
        public R second() { return right; }

        public override int GetHashCode()
        {
            return left.GetHashCode() ^ right.GetHashCode();
        }

        public override bool Equals(object o)
        {
            if (!(o is Pair<L, R>)) return false;
            Pair<L, R> pairo = (Pair<L, R>) o;
            return this.left.Equals(pairo.first()) &&
                this.right.Equals(pairo.second());
        }
    }

    bool pendingDent = true;

    // Current indent
    private int indentCount = 0;
    // A queue where extra tokens are pushed on
    private StackQueue<IToken> tokenQueue = new StackQueue<IToken>();
    // The stack that keeps key word and indent after that
    private Stack<Pair<string, int>> indentStack = new Stack<Pair<string, int>>();
    // Pointer keeps last indent token
    private IToken initialIndentToken = null;
    private string  lastKeyWord = "";

    private bool prevWasEndl = false;
    private bool prevWasKeyWord = false;
    // Need, for example, in {}-block
    private bool ignoreIndent = false;
    // Check moment, when you should calculate start indent
    // module ... where {now you should remember start indent}
    private bool moduleStartIndent = false;
    private bool wasModuleExport   = false;
    private bool inPragmas         = false;

    // Haskell saves indent before first() symbol as null indent
    private int startIndent = -1;
    // Count of "active" key words in this moment
    private int nestedLevel = 0;

    protected void processNEWLINEToken()
    {
        if (pendingDent) { this.Channel = Antlr4.Runtime.TokenConstants.HiddenChannel; }
        indentCount = 0;
        initialIndentToken = null;
    }

    protected void processTABToken()
    {
        this.Channel = Antlr4.Runtime.TokenConstants.HiddenChannel;
        if (pendingDent)
        {
            indentCount += 8 * this.Text.Length;
        }
    }

    protected void processWSToken()
    {
        this.Channel = Antlr4.Runtime.TokenConstants.HiddenChannel;
        if (pendingDent)
        {
            indentCount += this.Text.Length;
        }
    }

    private int getSavedIndent() { return !indentStack.Any() ? startIndent : indentStack.Peek().second(); }

    private CommonToken createToken(int type, string text, IToken next)
    {
        CommonToken token = new CommonToken(type, text);
        if (initialIndentToken != null)
        {
            token.StartIndex = initialIndentToken.StartIndex;
            token.Line = initialIndentToken.Line;
            token.Column = initialIndentToken.Column;
            token.StopIndex = next.StartIndex - 1;
        }
        return token;
    }

    private void processINToken(IToken next)
    {
        while (indentStack.Any() && !indentStack.Peek().first().Equals("let"))
        {
            tokenQueue.Push(createToken(HaskellLexer.SEMI, "SEMI", next));
            tokenQueue.Push(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
            nestedLevel--;
            indentStack.Pop();
        }

        if (indentStack.Any() && indentStack.Peek().first().Equals("let"))
        {
            tokenQueue.Push(createToken(HaskellLexer.SEMI, "SEMI", next));
            tokenQueue.Push(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
            nestedLevel--;
            indentStack.Pop();
        }
    }

    private void processEOFToken(IToken next)
    {
        indentCount = startIndent;
        if (!pendingDent)
        {
            initialIndentToken = next;
        }
        while (nestedLevel > indentStack.Count())
        {
            if (nestedLevel > 0)
                nestedLevel--;
            tokenQueue.Push(createToken(HaskellLexer.SEMI, "SEMI", next));
            tokenQueue.Push(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
        }
        while (indentCount < getSavedIndent())
        {
            if (indentStack.Any() && nestedLevel > 0)
            {
                indentStack.Pop();
                nestedLevel--;
            }
            tokenQueue.Push(createToken(HaskellLexer.SEMI, "SEMI", next));
            tokenQueue.Push(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
        }
        if (indentCount == getSavedIndent())
        {
            tokenQueue.Push(createToken(HaskellLexer.SEMI, "SEMI", next));
        }
        if (wasModuleExport)
        {
            tokenQueue.Push(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
        }
        startIndent = -1;
    }

    // Algorithm's description here:
    // https://www.haskell.org/onlinereport/haskell2010/haskellch10.html
    // https://en.wikibooks.org/wiki/Haskell/Indentation
    public override IToken NextToken()
    {
        if (tokenQueue.Any())
        {
            return tokenQueue.DequeueBottom();
        }
        IToken next = base.NextToken();
        var type = next.Type;
        if (type == HaskellLexer.OpenPragmaBracket)
        {
            inPragmas = true;
        }
        if (startIndent == -1
            && type != HaskellLexer.NEWLINE
            && type !=  HaskellLexer.WS
            && type !=  HaskellLexer.TAB
              && type != HaskellLexer.OCURLY)
        {
            if (type ==  HaskellLexer.MODULE)
            {
                moduleStartIndent = true;
                wasModuleExport = true;
            } if (type !=  HaskellLexer.MODULE && !moduleStartIndent && !inPragmas)
            {
                startIndent = next.Column;
            } else if (lastKeyWord.Equals("where") && moduleStartIndent)
            {
                lastKeyWord = "";
                prevWasKeyWord = false;
                nestedLevel = 0;
                moduleStartIndent = false;
                prevWasEndl = false;
                startIndent = next.Column;
                tokenQueue.Push(createToken(HaskellLexer.VOCURLY, "VOCURLY", next));
                tokenQueue.Push(createToken(type, next.Text, next));
                return tokenQueue.DequeueBottom();
            }
        }
        if (type == HaskellLexer.ClosePragmaBracket)
        {
            inPragmas = false;
        }
        if (type == HaskellLexer.OCURLY)
        {
            if (prevWasKeyWord)
            {
                nestedLevel--;
                prevWasKeyWord = false;
            }
            if (moduleStartIndent)
            {
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
              && type != HaskellLexer.OCURLY)
        {
            prevWasKeyWord = false;
            indentStack.Push(new Pair<string, int>(lastKeyWord, next.Column));
            tokenQueue.Push(createToken(HaskellLexer.VOCURLY, "VOCURLY", next));
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
           )
        {
            ignoreIndent = false;
        }
        if (pendingDent
            && prevWasKeyWord
            && !ignoreIndent
            && indentCount <= getSavedIndent()
            && type != HaskellLexer.NEWLINE
              && type !=  HaskellLexer.WS)
        {
            tokenQueue.Push(createToken(HaskellLexer.VOCURLY, "VOCURLY", next));
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
              && type != Antlr4.Runtime.TokenConstants.EOF)
        {
            while (nestedLevel > indentStack.Count())
            {
                if (nestedLevel > 0)
                    nestedLevel--;
                tokenQueue.Push(createToken(HaskellLexer.SEMI, "SEMI", next));
                tokenQueue.Push(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
            }
            while (indentCount < getSavedIndent())
            {
                if (indentStack.Any() && nestedLevel > 0)
                {
                    indentStack.Pop();
                    nestedLevel--;
                }
                tokenQueue.Push(createToken(HaskellLexer.SEMI, "SEMI", next));
                tokenQueue.Push(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
            }
            if (indentCount == getSavedIndent())
            {
                tokenQueue.Push(createToken(HaskellLexer.SEMI, "SEMI", next));
            }
            prevWasEndl = false;
            if (indentCount == startIndent)
            {
                pendingDent = false;
            }
        }
        if (pendingDent && prevWasKeyWord
            && !moduleStartIndent
            && !ignoreIndent
            && indentCount > getSavedIndent()
            && type != HaskellLexer.NEWLINE
            && type !=  HaskellLexer.WS
              && type != Antlr4.Runtime.TokenConstants.EOF)
        {
            prevWasKeyWord = false;
            if (prevWasEndl)
            {
                indentStack.Push(new Pair<string, int>(lastKeyWord, indentCount));
                prevWasEndl = false;
            }
            tokenQueue.Push(createToken(HaskellLexer.VOCURLY, "VOCURLY", next));
        }
        if (pendingDent
            && initialIndentToken == null
              && HaskellLexer.NEWLINE != type)
        {
            initialIndentToken = next;
        }
        if (next != null && type == HaskellLexer.NEWLINE)
        {
            prevWasEndl = true;
        }
        if (   type == HaskellLexer.WHERE
            || type == HaskellLexer.LET
            || type == HaskellLexer.DO
            || type == HaskellLexer.MDO
            || type == HaskellLexer.OF
            || type == HaskellLexer.LCASE
               || type == HaskellLexer.REC)
        {
            // if next will be HaskellLexer.OCURLY need to decrement nestedLevel
            nestedLevel++;
            prevWasKeyWord = true;
            prevWasEndl = false;
            lastKeyWord = next.Text;
            if (type == HaskellLexer.WHERE)
            {
                if (indentStack.Any() 
                    && (indentStack.Peek().first().Equals("do") 
                        || indentStack.Peek().first().Equals("mdo")))
                {
                    tokenQueue.Push(createToken(HaskellLexer.SEMI, "SEMI", next));
                    tokenQueue.Push(createToken(HaskellLexer.VCCURLY, "VCCURLY", next));
                    indentStack.Pop();
                    nestedLevel--;
                }
            }
        }
        if (next != null && type == HaskellLexer.OCURLY)
        {
            prevWasKeyWord = false;
        }
        if (next == null || Antlr4.Runtime.TokenConstants.HiddenChannel == next.Channel || HaskellLexer.NEWLINE == type)
        {
            return next;
        }
        if (type ==  HaskellLexer.IN)
        {
            processINToken(next);
        }
        if (type == Antlr4.Runtime.TokenConstants.EOF)
        {
            processEOFToken(next);
        }
        pendingDent = true;
        tokenQueue.Push(next);
        return tokenQueue.DequeueBottom();
    }
}
