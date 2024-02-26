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

using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using Antlr4.Runtime;

public abstract class PythonLexerBase : Lexer
{
    // A stack that keeps track of the indentation lengths
    private Stack<int> indentLengthStack;
    // A list where tokens are waiting to be loaded into the token stream
    private LinkedList<IToken> pendingTokens;
    // last pending token types
    private int previousPendingTokenType;
    private int lastPendingTokenTypeFromDefaultChannel;

    // The amount of opened parentheses, square brackets, or curly braces
    private int opened;

    private bool wasSpaceIndentation;
    private bool wasTabIndentation;
    private bool wasIndentationMixedWithSpacesAndTabs;
    private const int INVALID_LENGTH = -1;

    private CommonToken curToken; // current (under processing) token
    private IToken ffgToken;      // following (look ahead) token

    private const string ERR_TXT = " ERROR: ";

    protected PythonLexerBase(ICharStream input) : base(input)
    {
        this.Init();
    }

    protected PythonLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput) : base(input, output, errorOutput)
    {
        this.Init();
    }

    private void Init()
    {
        this.indentLengthStack = new Stack<int>();
        this.pendingTokens = new LinkedList<IToken>();
        this.previousPendingTokenType = 0;
        this.lastPendingTokenTypeFromDefaultChannel = 0;
        this.opened = 0;
        this.wasSpaceIndentation = false;
        this.wasTabIndentation = false;
        this.wasIndentationMixedWithSpacesAndTabs = false;
        this.curToken = null!;
        this.ffgToken = null!;
    }

    public override IToken NextToken() // reading the input stream until a return EOF
    {
        this.CheckNextToken();
        IToken firstPendingToken = this.pendingTokens.First.Value;
        this.pendingTokens.RemoveFirst();
        return firstPendingToken; // add the queued token to the token stream
    }

    private void CheckNextToken()
    {
        if (this.previousPendingTokenType != TokenConstants.EOF)
        {
            this.SetCurrentAndFollowingTokens();
            if (this.indentLengthStack.Count == 0) // We're at the first token
            {
                this.HandleStartOfInput();
            }

            switch (this.curToken.Type)
            {
                case PythonLexer.LPAR:
                case PythonLexer.LSQB:
                case PythonLexer.LBRACE:
                    this.opened++;
                    this.AddPendingToken(this.curToken);
                    break;
                case PythonLexer.RPAR:
                case PythonLexer.RSQB:
                case PythonLexer.RBRACE:
                    this.opened--;
                    this.AddPendingToken(this.curToken);
                    break;
                case PythonLexer.NEWLINE:
                    this.HandleNEWLINEtoken();
                    break;
                case PythonLexer.STRING:
                    this.HandleSTRINGtoken();
                    break;
                case PythonLexer.ERROR_TOKEN:
                    this.ReportLexerError("token recognition error at: '" + this.curToken.Text + "'");
                    this.AddPendingToken(this.curToken);
                    break;
                case TokenConstants.EOF:
                    this.HandleEOFtoken();
                    break;
                default:
                    this.AddPendingToken(this.curToken);
                    break;
            }
        }
    }

    private void SetCurrentAndFollowingTokens()
    {
        this.curToken = this.ffgToken == null ?
                    new CommonToken(base.NextToken()) :
                    new CommonToken(this.ffgToken);

        this.ffgToken = this.curToken.Type == TokenConstants.EOF ?
                    this.curToken :
                    base.NextToken();
    }

    // initialize the _indentLengths
    // hide the leading NEWLINE token(s)
    // if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    // insert a leading INDENT token if necessary
    private void HandleStartOfInput()
    {
        // initialize the stack with a default 0 indentation length
        this.indentLengthStack.Push(0); // this will never be popped off
        while (this.curToken.Type != TokenConstants.EOF)
        {
            if (this.curToken.Channel == TokenConstants.DefaultChannel)
            {
                if (this.curToken.Type == PythonLexer.NEWLINE)
                {
                    // all the NEWLINE tokens must be ignored before the first statement
                    this.HideAndAddPendingToken(this.curToken);
                }
                else
                { // We're at the first statement
                    this.InsertLeadingIndentToken();
                    return; // continue the processing of the current token with CheckNextToken()
                }
            }
            else
            {
                this.AddPendingToken(this.curToken); // it can be WS, EXPLICIT_LINE_JOINING, or COMMENT token
            }
            this.SetCurrentAndFollowingTokens();
        } // continue the processing of the EOF token with CheckNextToken()
    }

    private void InsertLeadingIndentToken()
    {
        if (this.previousPendingTokenType == PythonLexer.WS)
        {
            var prevToken = this.pendingTokens.Last.Value;
            if (this.GetIndentationLength(prevToken.Text) != 0) // there is an "indentation" before the first statement
            {
                const string errMsg = "first statement indented";
                this.ReportLexerError(errMsg);
                // insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
                this.CreateAndAddPendingToken(PythonLexer.INDENT, TokenConstants.DefaultChannel, PythonLexerBase.ERR_TXT + errMsg, this.curToken);
            }
        }
    }

    private void HandleNEWLINEtoken()
    {
        if (this.opened > 0)
        {
            // We're in an implicit line joining, ignore the current NEWLINE token
            this.HideAndAddPendingToken(this.curToken);
        }
        else
        {
            CommonToken nlToken = new CommonToken(this.curToken); // save the current NEWLINE token
            bool isLookingAhead = this.ffgToken.Type == PythonLexer.WS;
            if (isLookingAhead)
            {
                this.SetCurrentAndFollowingTokens(); // set the next two tokens
            }

            switch (this.ffgToken.Type)
            {
                case PythonLexer.NEWLINE:      // We're before a blank line
                case PythonLexer.COMMENT:      // We're before a comment
                    this.HideAndAddPendingToken(nlToken);
                    if (isLookingAhead)
                    {
                        this.AddPendingToken(this.curToken);  // WS token
                    }
                    break;
                default:
                    this.AddPendingToken(nlToken);
                    if (isLookingAhead)
                    { // We're on whitespace(s) followed by a statement
                        int indentationLength = this.ffgToken.Type == TokenConstants.EOF ?
                                                0 :
                                                this.GetIndentationLength(this.curToken.Text);

                        if (indentationLength != PythonLexerBase.INVALID_LENGTH)
                        {
                            this.AddPendingToken(this.curToken);  // WS token
                            this.InsertIndentOrDedentToken(indentationLength); // may insert INDENT token or DEDENT token(s)                            
                        }
                        else
                        {
                            this.ReportError("inconsistent use of tabs and spaces in indentation");
                        }
                    }
                    else
                    {
                        // We're at a newline followed by a statement (there is no whitespace before the statement)
                        this.InsertIndentOrDedentToken(0); // may insert DEDENT token(s)
                    }
                    break;
            }
        }
    }

    private void InsertIndentOrDedentToken(int indentLength)
    {
        //*** https://docs.python.org/3/reference/lexical_analysis.html#indentation
        int prevIndentLength = this.indentLengthStack.Peek();
        if (indentLength > prevIndentLength)
        {
            this.CreateAndAddPendingToken(PythonLexer.INDENT, TokenConstants.DefaultChannel, null, this.ffgToken);
            this.indentLengthStack.Push(indentLength);
        }
        else
        {
            while (indentLength < prevIndentLength)
            { // more than 1 DEDENT token may be inserted into the token stream
                this.indentLengthStack.Pop();
                prevIndentLength = this.indentLengthStack.Peek();
                if (indentLength <= prevIndentLength)
                {
                    this.CreateAndAddPendingToken(PythonLexer.DEDENT, TokenConstants.DefaultChannel, null, this.ffgToken);
                }
                else
                {
                    this.ReportError("inconsistent dedent");
                }
            }
        }
    }

    private void HandleSTRINGtoken()
    {
        // remove the \<newline> escape sequences from the string literal
        // https://docs.python.org/3.11/reference/lexical_analysis.html#string-and-bytes-literals
        string line_joinFreeStringLiteral = Regex.Replace(this.curToken.Text, @"\\\r?\n", "");
        if (this.curToken.Text.Length == line_joinFreeStringLiteral.Length)
        {
            this.AddPendingToken(this.curToken);
        }
        else
        {
            CommonToken originalSTRINGtoken = new CommonToken(this.curToken); // backup the original token
            this.curToken.Text = line_joinFreeStringLiteral;
            this.AddPendingToken(this.curToken);                  // add the modified token with inline string literal
            this.HideAndAddPendingToken(originalSTRINGtoken); // add the original token with a hidden channel
            // this inserted hidden token allows to restore the original string literal with the \<newline> escape sequences
        }
    }

    private void InsertTrailingTokens()
    {
        switch (this.lastPendingTokenTypeFromDefaultChannel)
        {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default:
                // insert an extra trailing NEWLINE token that serves as the end of the last statement
                this.CreateAndAddPendingToken(PythonLexer.NEWLINE, TokenConstants.DefaultChannel, null, this.ffgToken); // ffgToken is EOF
                break;
        }
        this.InsertIndentOrDedentToken(0); // Now insert as many trailing DEDENT tokens as needed
    }

    private void HandleEOFtoken()
    {
        if (this.lastPendingTokenTypeFromDefaultChannel > 0)
        { // there was a statement in the input (leading NEWLINE tokens are hidden)
            this.InsertTrailingTokens();
        }
        this.AddPendingToken(this.curToken);
    }

    private void HideAndAddPendingToken(CommonToken cToken)
    {
        cToken.Channel = TokenConstants.HiddenChannel;
        this.AddPendingToken(cToken);
    }

    private void CreateAndAddPendingToken(int type, int channel, string text, IToken baseToken)
    {
        CommonToken cToken = new CommonToken(baseToken);
        cToken.Type = type;
        cToken.Channel = channel;
        cToken.StopIndex = baseToken.StartIndex - 1;

        cToken.Text = text == null
                   ? "<" + Vocabulary.GetSymbolicName(type) + ">"
                   : text;

        this.AddPendingToken(cToken);
    }

    private void AddPendingToken(IToken token)
    {
        // save the last pending token type because the pendingTokens linked list can be empty by the nextToken()
        this.previousPendingTokenType = token.Type;
        if (token.Channel == TokenConstants.DefaultChannel)
        {
            this.lastPendingTokenTypeFromDefaultChannel = this.previousPendingTokenType;
        }
        this.pendingTokens.AddLast(token);
    }

    private int GetIndentationLength(string textWS) // the textWS may contain spaces, tabs or form feeds
    {
        const int TAB_LENGTH = 8; // the standard number of spaces to replace a tab with spaces
        int length = 0;
        foreach (char ch in textWS)
        {
            switch (ch)
            {
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

        if (this.wasTabIndentation && this.wasSpaceIndentation)
        {
            if (!this.wasIndentationMixedWithSpacesAndTabs)
            {
                this.wasIndentationMixedWithSpacesAndTabs = true;
                return PythonLexerBase.INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    private void ReportLexerError(string errMsg)
    {
        this.ErrorListenerDispatch.SyntaxError(this.ErrorOutput, this, this.curToken.Type, this.curToken.Line, this.curToken.Column, " LEXER" + PythonLexerBase.ERR_TXT + errMsg, null);
    }

    private void ReportError(string errMsg)
    {
        this.ReportLexerError(errMsg);

        // the ERROR_TOKEN will raise an error in the parser
        this.CreateAndAddPendingToken(PythonLexer.ERROR_TOKEN, TokenConstants.DefaultChannel, PythonLexerBase.ERR_TXT + errMsg, this.ffgToken);
    }

    public override void Reset()
    {
        this.Init();
        base.Reset();
    }
}
