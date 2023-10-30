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

using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using Antlr4.Runtime;

public abstract class PythonLexerBase : Lexer
{
    // A stack that keeps track of the indentation lengths
    private readonly LinkedList<int> _indentLengths = new LinkedList<int>();
    // A linked list where tokens are waiting to be loaded into the token stream
    private readonly LinkedList<IToken> _pendingTokens = new LinkedList<IToken>();
    // last pending token types
    private int _previousPendingTokenType = 0;
    private int _lastPendingTokenTypeForDefaultChannel = 0;

    // The amount of opened parentheses, square brackets, or curly braces
    private int _opened = 0;

    private bool _wasSpaceIndentation = false;
    private bool _wasTabIndentation = false;
    private bool _wasIndentationMixedWithSpacesAndTabs = false;
    private const int INVALID_LENGTH = -1;

    private CommonToken _curToken = null!; // current (under processing) token
    private IToken _ffgToken = null!;      // following (look ahead) token

    private const string _ERR_TXT = " ERROR: ";

    protected PythonLexerBase(ICharStream input) : base(input)
    {
    }

    protected PythonLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput) : base(input, output, errorOutput)
    {
    }

    public override IToken NextToken() // reading the input stream until a return EOF
    {
        CheckNextToken();
        IToken firstPendingToken = _pendingTokens.First.Value;
        _pendingTokens.RemoveFirst();
        return firstPendingToken; // add the queued token to the token stream
    }

    private void CheckNextToken()
    {
        if (_previousPendingTokenType != Eof)
        {
            SetCurrentAndFollowingTokens();
            if (_indentLengths.Count == 0) // We're at the first token
            {
                HandleStartOfInput();
            }

            switch (_curToken.Type)
            {
                case PythonLexer.LPAR:
                case PythonLexer.LSQB:
                case PythonLexer.LBRACE:
                    _opened++;
                    AddPendingToken(_curToken);
                    break;
                case PythonLexer.RPAR:
                case PythonLexer.RSQB:
                case PythonLexer.RBRACE:
                    _opened--;
                    AddPendingToken(_curToken);
                    break;
                case PythonLexer.NEWLINE:
                    HandleNEWLINEtoken();
                    break;
                case PythonLexer.STRING:
                    HandleSTRINGtoken();
                    break;
                case PythonLexer.ERROR_TOKEN:
                    ReportLexerError("token recognition error at: '" + _curToken.Text + "'");
                    AddPendingToken(_curToken);
                    break;
                case Eof:
                    HandleEOFtoken();
                    break;
                default:
                    AddPendingToken(_curToken);
                    break;
            }
        }
    }

    private void SetCurrentAndFollowingTokens()
    {
        _curToken = _ffgToken == null ?
                    new CommonToken(base.NextToken()) :
                    new CommonToken(_ffgToken);

        _ffgToken = _curToken.Type == Eof ?
                    _curToken :
                    base.NextToken();
    }

    // initialize the _indentLengths stack
    // hide the leading NEWLINE token(s)
    // if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    // insert a leading INDENT token if necessary
    private void HandleStartOfInput()
    {
        // initialize the stack with a default 0 indentation length
        _indentLengths.AddLast(0); // this will never be popped off
        while (_curToken.Type != Eof)
        {
            if (_curToken.Channel == TokenConstants.DefaultChannel)
            {
                if (_curToken.Type == PythonLexer.NEWLINE)
                {
                    // all the NEWLINE tokens must be ignored before the first statement
                    HideAndAddPendingToken(_curToken);
                }
                else
                { // We're at the first statement
                    InsertLeadingIndentToken();
                    return; // continue the processing of the current token with CheckNextToken()
                }
            }
            else
            {
                AddPendingToken(_curToken); // it can be WS, EXPLICIT_LINE_JOINING, or COMMENT token
            }
            SetCurrentAndFollowingTokens();
        } // continue the processing of the EOF token with CheckNextToken()
    }

    private void InsertLeadingIndentToken()
    {
        if (_previousPendingTokenType == PythonLexer.WS)
        {
            var prevToken = _pendingTokens.Last.Value;
            if (GetIndentationLength(prevToken.Text) != 0) // there is an "indentation" before the first statement
            {
                const string errMsg = "first statement indented";
                ReportLexerError(errMsg);
                // insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
                CreateAndAddPendingToken(PythonLexer.INDENT, TokenConstants.DefaultChannel, _ERR_TXT + errMsg, _curToken);
            }
        }
    }

    private void HandleNEWLINEtoken()
    {
        if (_opened > 0)
        {
            // We're in an implicit line joining, ignore the current NEWLINE token
            HideAndAddPendingToken(_curToken);
        }
        else
        {
            CommonToken nlToken = (CommonToken)_curToken; // save the current NEWLINE token
            bool isLookingAhead = _ffgToken.Type == PythonLexer.WS;
            if (isLookingAhead)
            {
                SetCurrentAndFollowingTokens(); // set the two next tokens
            }

            switch (_ffgToken.Type)
            {
                case PythonLexer.NEWLINE:      // We're before a blank line
                case PythonLexer.COMMENT:      // We're before a comment
                    HideAndAddPendingToken(nlToken);
                    if (isLookingAhead)
                    {
                        AddPendingToken(_curToken);  // WS token
                    }
                    break;
                default:
                    AddPendingToken(nlToken);
                    if (isLookingAhead)
                    { // We're on whitespace(s) followed by a statement
                        int indentationLength = _ffgToken.Type == Eof ?
                                                0 :
                                                GetIndentationLength(_curToken.Text);

                        if (indentationLength != INVALID_LENGTH)
                        {
                            AddPendingToken(_curToken);  // WS token
                            InsertIndentOrDedentToken(indentationLength); // may insert INDENT token or DEDENT token(s)                            
                        }
                        else
                        {
                            ReportError("inconsistent use of tabs and spaces in indentation");
                        }
                    }
                    else
                    {
                        // We're at a newline followed by a statement (there is no whitespace before the statement)
                        InsertIndentOrDedentToken(0); // may insert DEDENT token(s)
                    }
                    break;
            }
        }
    }

    private void InsertIndentOrDedentToken(int curIndentLength)
    {
        //*** https://docs.python.org/3/reference/lexical_analysis.html#indentation
        int prevIndentLength = _indentLengths.Last.Value;
        if (curIndentLength > prevIndentLength)
        {
            CreateAndAddPendingToken(PythonLexer.INDENT, TokenConstants.DefaultChannel, null, _ffgToken);
            _indentLengths.AddLast(curIndentLength);
        }
        else
        {
            while (curIndentLength < prevIndentLength)
            { // more than 1 DEDENT token may be inserted into the token stream
                _indentLengths.RemoveLast();
                prevIndentLength = _indentLengths.Last.Value;
                if (curIndentLength <= prevIndentLength)
                {
                    CreateAndAddPendingToken(PythonLexer.DEDENT, TokenConstants.DefaultChannel, null, _ffgToken);
                }
                else
                {
                    ReportError("inconsistent dedent");
                }
            }
        }
    }

    private void HandleSTRINGtoken()
    {
        // remove the \<newline> escape sequences from the string literal
        // https://docs.python.org/3.11/reference/lexical_analysis.html#string-and-bytes-literals
        string line_joinFreeStringLiteral = Regex.Replace(_curToken.Text, @"\\\r?\n", "");
        if (_curToken.Text.Length == line_joinFreeStringLiteral.Length)
        {
            AddPendingToken(_curToken);
        }
        else
        {
            CommonToken originalSTRINGtoken = new CommonToken(_curToken); // backup the original token
            _curToken.Text = line_joinFreeStringLiteral;
            AddPendingToken(_curToken);                  // add the modified token with inline string literal
            HideAndAddPendingToken(originalSTRINGtoken); // add the original token with a hidden channel
            // this inserted hidden token allows to restore the original string literal with the \<newline> escape sequences
        }
    }

    private void InsertTrailingTokens()
    {
        switch (_lastPendingTokenTypeForDefaultChannel)
        {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default:
                // insert an extra trailing NEWLINE token that serves as the end of the last statement
                CreateAndAddPendingToken(PythonLexer.NEWLINE, TokenConstants.DefaultChannel, null, _ffgToken); // _ffgToken is EOF
                break;
        }
        InsertIndentOrDedentToken(0); // Now insert as many trailing DEDENT tokens as needed
    }

    private void HandleEOFtoken()
    {
        if (_lastPendingTokenTypeForDefaultChannel > 0)
        { // there was a statement in the input (leading NEWLINE tokens are hidden)
            InsertTrailingTokens();
        }
        AddPendingToken(_curToken);
    }

    private void HideAndAddPendingToken(CommonToken token)
    {
        token.Channel = TokenConstants.HiddenChannel;
        AddPendingToken(token);
    }

    private void CreateAndAddPendingToken(int type, int channel, string text, IToken baseToken)
    {
        CommonToken token = new CommonToken((CommonToken)baseToken);
        token.Type = type;
        token.Channel = channel;
        token.StopIndex = baseToken.StartIndex - 1;

        token.Text = text == null
                   ? "<" + Vocabulary.GetSymbolicName(type) + ">"
                   : text;

        AddPendingToken(token);
    }

    private void AddPendingToken(IToken token)
    {
        // save the last pending token type because the _pendingTokens linked list can be empty by the nextToken()
        _previousPendingTokenType = token.Type;
        if (token.Channel == TokenConstants.DefaultChannel)
        {
            _lastPendingTokenTypeForDefaultChannel = _previousPendingTokenType;
        }
        _pendingTokens.AddLast(token);
    }

    private int GetIndentationLength(string textWS) // the textWS may contain spaces, tabs or formfeeds
    {
        const int TAB_LENGTH = 8; // the standard number of spaces to replace a tab with spaces
        int length = 0;
        foreach (char ch in textWS)
        {
            switch (ch)
            {
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

        if (_wasTabIndentation && _wasSpaceIndentation)
        {
            if (!_wasIndentationMixedWithSpacesAndTabs)
            {
                _wasIndentationMixedWithSpacesAndTabs = true;
                return INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    private void ReportLexerError(string errMsg)
    {
        ErrorListenerDispatch.SyntaxError(ErrorOutput, this, _curToken.Type, _curToken.Line, _curToken.Column, _ERR_TXT + errMsg, null);
    }

    private void ReportError(string errMsg)
    {
        ReportLexerError(errMsg);

        // the ERROR_TOKEN will raise an error in the parser
        CreateAndAddPendingToken(PythonLexer.ERROR_TOKEN, TokenConstants.DefaultChannel, _ERR_TXT + errMsg, _ffgToken);
    }
}
