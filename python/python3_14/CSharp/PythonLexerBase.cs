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
 * Project : A helper class for an ANTLR4 Python lexer grammar that assists in tokenizing indentation,
 *           interpolated strings, and encoding declaration.
 *
 * Developed by : Robert Einhorn
 */

using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Collections.Frozen;
using System.IO;


[assembly: CLSCompliant(true)]

public abstract class PythonLexerBase : Lexer
{
    private const int INVALID_LENGTH = -1;
    private const string ERR_TXT = " ERROR: ";
    private const int TAB_LENGTH = 8;
    private static readonly FrozenDictionary<string, int> LEXER_MODES_FOR_ISTRING_START =
    new Dictionary<string, int>
    {
        // f-strings
        { "f'", PythonLexer.SQ1__FSTRING_MODE },
        { "rf'", PythonLexer.SQ1R_FSTRING_MODE },
        { "fr'", PythonLexer.SQ1R_FSTRING_MODE },
        { "f\"", PythonLexer.DQ1__FSTRING_MODE },
        { "rf\"", PythonLexer.DQ1R_FSTRING_MODE },
        { "fr\"", PythonLexer.DQ1R_FSTRING_MODE },
        { "f'''", PythonLexer.SQ3__FSTRING_MODE },
        { "rf'''", PythonLexer.SQ3R_FSTRING_MODE },
        { "fr'''", PythonLexer.SQ3R_FSTRING_MODE },
        { "f\"\"\"", PythonLexer.DQ3__FSTRING_MODE },
        { "rf\"\"\"", PythonLexer.DQ3R_FSTRING_MODE },
        { "fr\"\"\"", PythonLexer.DQ3R_FSTRING_MODE },

        // t-strings
        { "t'", PythonLexer.SQ1__TSTRING_MODE },
        { "rt'", PythonLexer.SQ1R_TSTRING_MODE },
        { "tr'", PythonLexer.SQ1R_TSTRING_MODE },
        { "t\"", PythonLexer.DQ1__TSTRING_MODE },
        { "rt\"", PythonLexer.DQ1R_TSTRING_MODE },
        { "tr\"", PythonLexer.DQ1R_TSTRING_MODE },
        { "t'''", PythonLexer.SQ3__TSTRING_MODE },
        { "rt'''", PythonLexer.SQ3R_TSTRING_MODE },
        { "tr'''", PythonLexer.SQ3R_TSTRING_MODE },
        { "t\"\"\"", PythonLexer.DQ3__TSTRING_MODE },
        { "rt\"\"\"", PythonLexer.DQ3R_TSTRING_MODE },
        { "tr\"\"\"", PythonLexer.DQ3R_TSTRING_MODE }
    }.ToFrozenDictionary();

    private string encodingName = "";

    // Indentation handling
    private Stack<int> indentationLengthStack = new();
    private LinkedList<IToken> pendingTokenQueue = new();

    private int previousPendingTokenType;
    private int lastPendingTokenTypeFromDefaultChannel;

    // Parenthesis / bracket / brace counts
    private int openParenBracketBraceCount;
    private Stack<int> paren_or_bracket_openedStack = new();
    private Stack<string> braceExpressionStack = new();
    private string lastClosedBraceExpression = "";

    // Current interpolated STRING_MIDDLE token type (FSTRING_MIDDLE or TSTRING_MIDDLE)
    private int activeInterpolatedStringMiddleTokenType;

    // We reimplement mode/stack because not all runtimes expose _mode/_modeStack
    private int curLexerMode;
    private Stack<int> lexerModeStack = new();

    // Indentation diagnostics
    private bool wasSpaceIndentation;
    private bool wasTabIndentation;
    private bool hasMixedIndentationBeenReported;

    // Current / lookahead tokens
    private IToken curToken = null!;
    private IToken laToken = null!;

    protected PythonLexerBase(ICharStream input)
        : this(input, Console.Out, Console.Error) { }

    protected PythonLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) { }

    public override void Reset()
    {
        this.Init();
        base.Reset();
    }

    private void Init()
    {
        this.encodingName = "";
        this.indentationLengthStack = new();
        this.pendingTokenQueue = new();
        this.previousPendingTokenType = 0;
        this.lastPendingTokenTypeFromDefaultChannel = 0;
        this.openParenBracketBraceCount = 0;
        this.paren_or_bracket_openedStack = new();
        this.braceExpressionStack = new();
        this.lastClosedBraceExpression = "";
        this.activeInterpolatedStringMiddleTokenType = 0;
        this.curLexerMode = Lexer.DEFAULT_MODE;
        this.lexerModeStack = new();
        this.wasSpaceIndentation = false;
        this.wasTabIndentation = false;
        this.hasMixedIndentationBeenReported = false;
        this.curToken = null!;
        this.laToken = null!;
    }

    /// <summary>
    /// Sets the encoding name to emit an ENCODING token at the start of the token stream.
    /// Leave empty if not needed (e.g., when parsing from string).
    /// </summary>
    /// <param name="encodingName">
    /// The encoding name (e.g., "utf-8"), or empty string to disable ENCODING token.
    /// </param>
    public void SetEncodingName(string encodingName)
    {
        this.encodingName = encodingName;
    }

    public override IToken NextToken() // Reading the input stream until EOF is reached
    {
        this.ProcessCurrentToken();
        IToken firstPendingToken = this.pendingTokenQueue.First!.Value;
        this.pendingTokenQueue.RemoveFirst();
        return firstPendingToken; // Add the queued token to the token stream
    }

    private void ProcessCurrentToken()
    {
        if (this.previousPendingTokenType == TokenConstants.EOF)
            return;

        this.SetCurrentAndFollowingTokens();
        if (this.indentationLengthStack.Count == 0) // We're at the first token
        {
            this.HandleStartOfInput();
        }

        switch (this.curToken.Type)
        {
            case PythonLexer.NEWLINE:
                this.HandleNEWLINEtoken();
                break;
            case PythonLexer.LPAR:
            case PythonLexer.LSQB:
            case PythonLexer.LBRACE:
                this.openParenBracketBraceCount++;
                this.AddPendingToken(this.curToken);
                break;
            case PythonLexer.RPAR:
            case PythonLexer.RSQB:
            case PythonLexer.RBRACE:
                this.openParenBracketBraceCount--;
                this.AddPendingToken(this.curToken);
                break;
            case PythonLexer.FSTRING_MIDDLE:
            case PythonLexer.TSTRING_MIDDLE:
                this.HandleISTRING_MIDDLEtokenWithDoubleBrace(); // does not affect the opened field
                this.AddPendingToken(this.curToken);
                break;
            case PythonLexer.COLONEQUAL:
                this.HandleCOLONEQUALtokenInIString();
                break;
            case PythonLexer.ERRORTOKEN:
                ReportLexerError($"token recognition error at: '{curToken.Text}'");
                this.AddPendingToken(this.curToken);
                break;
            case TokenConstants.EOF:
                this.HandleEOFtoken();
                break;
            default:
                this.AddPendingToken(this.curToken);
                break;
        }
        this.HandleFORMAT_SPECIFICATION_MODE();
    }

    private void SetCurrentAndFollowingTokens()
    {
        this.curToken = this.laToken == null ?
                        base.NextToken() :
                        this.laToken;

        this.NormalizeCurToken(); // Do not use laToken in this method or any of its submethods — it hasn't been set yet!

        this.laToken = this.curToken.Type == TokenConstants.EOF ?
                        this.curToken :
                        base.NextToken();
    }

    // ===================== Leading‑Token Preprocessing =====================
    // Handles BOM skipping, ENCODING token insertion, suppression of leading
    // NEWLINE tokens, and validation of the first INDENT before normal token
    // processing begins.

    private void HandleStartOfInput()
    {
        // - initialize indent stack with a default 0 indentation length
        // - skip BOM token
        // - insert ENCODING token (if any)
        // - hide leading NEWLINE(s)
        // - insert leading INDENT if first statement is indented
        this.indentationLengthStack.Push(0); // this will never be popped off

        if (this.curToken.Type == PythonLexer.BOM)
        {
            this.SetCurrentAndFollowingTokens();
        }
        this.InsertENCODINGtoken();

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
                    return; // continue the processing of the current token with ProcessCurrentToken()
                }
            }
            else
            {
                this.AddPendingToken(this.curToken); // it can be WS, EXPLICIT_LINE_JOINING, or COMMENT token
            }
            this.SetCurrentAndFollowingTokens();
        } // continue the processing of the EOF token with ProcessCurrentToken()
    }

    private void InsertENCODINGtoken() // https://peps.python.org/pep-0263/
    {
        if (this.encodingName == "") return;

        var sourcePair = new Tuple<ITokenSource, ICharStream>(this, (ICharStream)this.InputStream);
        var encodingToken = new CommonToken(sourcePair, PythonLexer.ENCODING, TokenConstants.HiddenChannel, start: 0, stop: 0);
        encodingToken.Text = this.encodingName;
        encodingToken.Line = 0;
        encodingToken.Column = -1;
        AddPendingToken(encodingToken);
    }

    private void InsertLeadingIndentToken()
    {
        if (this.previousPendingTokenType == PythonLexer.WS)
        {
            var prevToken = this.pendingTokenQueue.Last!.Value;
            if (this.GetIndentationLength(prevToken.Text) != 0) // there is an "indentation" before the first statement
            {
                const string errMsg = "first statement indented";
                this.ReportLexerError(errMsg);
                // insert an INDENT token before the first statement to trigger an 'unexpected indent' error later in the parser
                this.CreateAndAddPendingToken(PythonLexer.INDENT, PythonLexerBase.ERR_TXT + errMsg, this.curToken);
            }
        }
    }

    // ===================== Indentation Handling =====================
    // Processes NEWLINE tokens, computes indentation length from leading
    // whitespace, manages the INDENT/DEDENT stack, emits the appropriate
    // indentation tokens, and detects inconsistent mixing of tabs and spaces

    private void HandleNEWLINEtoken()
    {
        if (this.lexerModeStack.Count > 0) // for multi line f/t-string literals
        {
            this.AddPendingToken(this.curToken);
            return;
        }

        if (this.openParenBracketBraceCount > 0)
        {
            // We're in an implicit line joining, ignore the current NEWLINE token
            this.HideAndAddPendingToken(this.curToken);
            return;
        }

        var nlToken = new CommonToken(this.curToken); // save the current NEWLINE token
        var isLookingAhead = this.laToken.Type == PythonLexer.WS;
        if (isLookingAhead)
        {
            this.SetCurrentAndFollowingTokens(); // set the next two tokens
        }

        switch (this.laToken.Type)
        {
            case PythonLexer.NEWLINE: // We're before a blank line
            case PythonLexer.COMMENT: // We're before a comment
                this.HideAndAddPendingToken(nlToken);
                if (isLookingAhead)
                {
                    this.AddPendingToken(this.curToken); // WS token
                }
                break;
            default:
                this.AddPendingToken(nlToken);
                if (isLookingAhead)
                { // We're on a whitespace(s) followed by a statement
                    var indentationLength = this.laToken.Type == TokenConstants.EOF ?
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

    private void InsertIndentOrDedentToken(int indentLength)
    {
        var prevIndentLength = this.indentationLengthStack.Peek();
        if (indentLength > prevIndentLength)
        {
            this.CreateAndAddPendingToken(PythonLexer.INDENT, null, this.laToken);
            this.indentationLengthStack.Push(indentLength);
            return;
        }

        while (indentLength < prevIndentLength)
        { // more than 1 DEDENT token may be inserted into the token stream
            this.indentationLengthStack.Pop();
            prevIndentLength = this.indentationLengthStack.Peek();
            if (indentLength <= prevIndentLength)
            {
                this.CreateAndAddPendingToken(PythonLexer.DEDENT, null, this.laToken);
            }
            else
            {
                this.ReportError("inconsistent dedent");
            }
        }
    }

    private int GetIndentationLength(string indentText) // the indentText may contain spaces, tabs or form feeds
    {
        var length = 0;
        foreach (char ch in indentText)
        {
            switch (ch)
            {
                case ' ':
                    this.wasSpaceIndentation = true;
                    length += 1;
                    break;
                case '\t':
                    this.wasTabIndentation = true;
                    length += PythonLexerBase.TAB_LENGTH - (length % PythonLexerBase.TAB_LENGTH);
                    break;
                case '\f': // form feed
                    length = 0;
                    break;
            }
        }

        if (this.wasTabIndentation && this.wasSpaceIndentation)
        {
            if (!this.hasMixedIndentationBeenReported)
            {
                this.hasMixedIndentationBeenReported = true;
                length = PythonLexerBase.INVALID_LENGTH; // only for the first inconsistent indent
            }
        }
        return length;
    }

    // ===================== IString Processing =====================
    // Handles f- and t-string interpolation, including prefix detection,
    // ISTRING_MIDDLE token rewriting, escaped brace handling ({{ and }}),
    // quote+brace boundary cases, format-specifier mode transitions,
    // and brace-expression accumulation inside interpolated segments.

    private void NormalizeCurToken()
    {
        switch (this.curToken.Type)
        {
            case PythonLexer.FSTRING_START:
                this.activeInterpolatedStringMiddleTokenType = PythonLexer.FSTRING_MIDDLE;
                this.SetLexerModeByISTRING_STARTtoken();
                return;
            case PythonLexer.TSTRING_START:
                this.activeInterpolatedStringMiddleTokenType = PythonLexer.TSTRING_MIDDLE;
                this.SetLexerModeByISTRING_STARTtoken();
                return;
            case PythonLexer.FSTRING_MIDDLE:
            case PythonLexer.TSTRING_MIDDLE:
                this.HandleISTRING_MIDDLEtokenWithQuoteAndLBrace(); // affect the opened field
                switch (this.curToken.Type)
                {
                    case PythonLexer.FSTRING_MIDDLE:
                    case PythonLexer.TSTRING_MIDDLE:
                        return; // No curToken exchange happened
                }
                break;
            case PythonLexer.FSTRING_END:
            case PythonLexer.TSTRING_END:
                this.PopLexerMode();
                return;
            default:
                if (this.lexerModeStack.Count == 0)
                {
                    return; // Not in f/t-string mode
                }
                break;
        }
        this.ProcessBraceExpression();
    }

    private void ProcessBraceExpression()
    {
        switch (this.curToken.Type) // the following tokens can only come from default mode (after an LBRACE in f/t-string)
        {
            case PythonLexer.NEWLINE:
                // append the current brace expression with the current newline
                this.AppendToBraceExpression(this.curToken.Text);
                var nlToken = new CommonToken(this.curToken);
                nlToken.Channel = TokenConstants.HiddenChannel;
                this.curToken = nlToken;
                break;
            case PythonLexer.LBRACE:
                // the outermost brace expression cannot be a dictionary comprehension or a set comprehension
                this.braceExpressionStack.Push("{");
                this.paren_or_bracket_openedStack.Push(0);
                this.PushLexerMode(Lexer.DEFAULT_MODE);
                break;
            case PythonLexer.LPAR:
            case PythonLexer.LSQB:
                // append the current brace expression with a "(" or a "["
                this.AppendToBraceExpression(this.curToken.Text);
                // https://peps.python.org/pep-0498/#lambdas-inside-expressions
                this.IncrementBraceDepth();
                break;
            case PythonLexer.RPAR:
            case PythonLexer.RSQB:
                // append the current brace expression with a ")" or a "]"
                this.AppendToBraceExpression(this.curToken.Text);
                this.DecrementBraceDepth();
                break;
            case PythonLexer.COLON:
            case PythonLexer.COLONEQUAL:
                // append the current brace expression with a ":" or a ":="
                this.AppendToBraceExpression(this.curToken.Text);
                this.SetLexerModeByCOLONorCOLONEQUALtoken();
                break;
            case PythonLexer.RBRACE:
                this.SetLexerModeAfterRBRACEtoken();
                break;
            default:
                // append the current brace expression with the current token text
                this.AppendToBraceExpression(this.curToken.Text);
                break;
        }
    }

    private void AppendToBraceExpression(string text)
    {
        var top = this.braceExpressionStack.Pop();
        this.braceExpressionStack.Push(top + text);
    }

    private void IncrementBraceDepth()
    { // increment the last element
        var top = this.paren_or_bracket_openedStack.Pop();
        this.paren_or_bracket_openedStack.Push(top + 1);
    }

    private void DecrementBraceDepth()
    { // decrement the last element
        var top = this.paren_or_bracket_openedStack.Pop();
        this.paren_or_bracket_openedStack.Push(top - 1);
    }

    private void SetLexerModeAfterRBRACEtoken()
    {
        switch (this.curLexerMode)
        {
            case Lexer.DEFAULT_MODE:
                this.PopLexerMode();
                this.PopByBRACE();
                break;
            case PythonLexer.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.PopLexerMode();
                this.PopLexerMode();
                this.PopByBRACE();
                break;
            default:
                this.ReportLexerError("f-string: single '}' is not allowed");
                break;
        }
    }

    private void SetLexerModeByISTRING_STARTtoken() // ISTRING = interpolated string (FSTRING or TSTRING)
    {
        var interpolatedStringPrefix = this.curToken.Text.ToLower();
        if (PythonLexerBase.LEXER_MODES_FOR_ISTRING_START.TryGetValue(interpolatedStringPrefix, out int newLexerMode))
        {
            this.PushLexerMode(newLexerMode);
        }
        else
        {
            this.ReportLexerError($"internal error: unknown interpolated string literal prefix: {this.curToken.Text}");
        }
    }

    private void SetLexerModeByCOLONorCOLONEQUALtoken()
    {
        // Exit early when the current lexer mode indicates an open parenthesis/bracket
        var opened = this.paren_or_bracket_openedStack.Peek() != 0;
        if (opened) return;

        // COLONEQUAL token will be replaced with a COLON token in ProcessCurrentToken()
        var prevLexerMode = this.lexerModeStack.Peek();
        switch (prevLexerMode) // check the previous lexer mode (the current is DEFAULT_MODE)
        {
            case PythonLexer.SQ1__FSTRING_MODE:
            case PythonLexer.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.SQ1__TSTRING_MODE:
            case PythonLexer.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.SQ1R_FSTRING_MODE:
            case PythonLexer.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.SQ1R_TSTRING_MODE:
            case PythonLexer.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.DQ1__FSTRING_MODE:
            case PythonLexer.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.DQ1__TSTRING_MODE:
            case PythonLexer.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.DQ1R_FSTRING_MODE:
            case PythonLexer.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.DQ1R_TSTRING_MODE:
            case PythonLexer.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.SQ3__FSTRING_MODE:
            case PythonLexer.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.SQ3__TSTRING_MODE:
            case PythonLexer.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.SQ3R_FSTRING_MODE:
            case PythonLexer.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.SQ3R_TSTRING_MODE:
            case PythonLexer.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.DQ3__FSTRING_MODE:
            case PythonLexer.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.DQ3__TSTRING_MODE:
            case PythonLexer.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.DQ3R_FSTRING_MODE:
            case PythonLexer.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;

            case PythonLexer.DQ3R_TSTRING_MODE:
            case PythonLexer.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
                this.PushLexerMode(PythonLexer.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                break;
        }
    }

    private void PopByBRACE()
    {
        this.paren_or_bracket_openedStack.Pop();
        var curBraceExpression = this.braceExpressionStack.Pop();
        this.lastClosedBraceExpression = curBraceExpression + "}";
        if (this.braceExpressionStack.Count > 0)
        {
            // Extend the current brace expression by adding the previous expression
            curBraceExpression = this.braceExpressionStack.Pop();
            this.braceExpressionStack.Push(curBraceExpression + this.lastClosedBraceExpression);
        }
    }

    private void HandleISTRING_MIDDLEtokenWithDoubleBrace() // ISTRING = interpolated string (FSTRING or TSTRING)
    {
        // replace the trailing double brace with a single brace and insert a hidden brace token
        var lastTwoChars = this.GetLastTwoCharsOfTheCurTokenText();
        switch (lastTwoChars)
        {
            case "{{":
                this.TrimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", TokenConstants.HiddenChannel);
                break;
            case "}}":
                this.TrimLastCharAddPendingTokenSetCurToken(PythonLexer.RBRACE, "}", TokenConstants.HiddenChannel);
                break;
        }
    }

    private void HandleISTRING_MIDDLEtokenWithQuoteAndLBrace() // ISTRING = interpolated string (FSTRING or TSTRING)
    {
        // replace the trailing     quote + left_brace with a quote     and insert an LBRACE token
        // replace the trailing backslash + left_brace with a backslash and insert an LBRACE token
        var lastTwoChars = this.GetLastTwoCharsOfTheCurTokenText();
        switch (lastTwoChars)
        {
            case "\"{":
            case "'{":
            case "\\{":
                this.TrimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", TokenConstants.DefaultChannel);
                break;
        }
    }

    private string GetLastTwoCharsOfTheCurTokenText()
    {
        var text = this.curToken.Text;
        return text.Length >= 2 ? text[^2..] : text;

    }

    private void TrimLastCharAddPendingTokenSetCurToken(int type, string text, int channel)
    {
        // trim the last char and add the modified curToken to the pendingTokenQueue
        var curTokenText = this.curToken.Text;
        var tokenTextWithoutLastChar = curTokenText[..^1];
        var token = new CommonToken(this.curToken);
        token.Text = tokenTextWithoutLastChar;
        token.StopIndex -= 1;
        this.AddPendingToken(token);

        this.ReplaceCurrentToken(type, text, channel); // set curToken
    }

    private void HandleCOLONEQUALtokenInIString() // ISTRING = interpolated string (FSTRING or TSTRING)
    {
        if (this.lexerModeStack.Count > 0 &&
            this.paren_or_bracket_openedStack.Peek() == 0)
        {
            // In an f/t-string, the walrus operator (:=) is only allowed inside parentheses.
            // If used outside, split the COLONEQUAL token into a COLON
            // (used as a format specifier instead of a walrus operator),
            // and move the equal sign to the beginning of the next token (FSTRING_MIDDLE or TSTRING_MIDDLE).
            var colonequalToken = new CommonToken(this.curToken);
            colonequalToken.Type = PythonLexer.COLON;
            colonequalToken.Text = ":";
            colonequalToken.StopIndex = colonequalToken.StartIndex;
            this.curToken = colonequalToken;

            switch (this.laToken.Type)
            {
                case PythonLexer.FSTRING_MIDDLE:
                case PythonLexer.TSTRING_MIDDLE:
                    colonequalToken = new CommonToken(this.laToken);
                    colonequalToken.Text = "=" + colonequalToken.Text;
                    colonequalToken.StartIndex -= 1;
                    colonequalToken.Column -= 1;
                    this.laToken = colonequalToken;
                    break;
                default:
                    this.AddPendingToken(this.curToken);
                    this.ReplaceCurrentToken(this.activeInterpolatedStringMiddleTokenType, "=", TokenConstants.DefaultChannel);
                    break;
            }
        }
        this.AddPendingToken(this.curToken);
    }

    private void ReplaceCurrentToken(int type, string text, int channel)
    {
        var token = new CommonToken(this.curToken);
        token.Type = type;
        token.Text = text;
        token.Channel = channel;
        token.Column += 1;
        token.StartIndex += 1;
        token.StopIndex = token.StartIndex;
        this.curToken = token;
    }

    // ===================== Lexer‑Mode Management =====================
    // Manages the custom lexer mode stack used for interpolated strings.
    // Provides controlled push/pop operations independent of ANTLR’s
    // internal mode stack to ensure correct behavior in complex IString flows.

    private void PushLexerMode(int mode)
    {
        this.PushMode(mode);
        this.lexerModeStack.Push(this.curLexerMode);
        this.curLexerMode = mode;
    }

    private void PopLexerMode()
    {
        this.PopMode();
        this.curLexerMode = this.lexerModeStack.Pop();
    }

    // ===================== Format‑Specifier Handling =====================

    private void HandleFORMAT_SPECIFICATION_MODE()
    {
        if (this.lexerModeStack.Count == 0 || this.laToken.Type != PythonLexer.RBRACE)
        {
            return;
        }

        // insert an empty FSTRING_MIDDLE or TSTRING_MIDDLE token instead of the missing format specification
        switch (this.curToken.Type)
        {
            case PythonLexer.COLON:
                this.CreateAndAddPendingToken(this.activeInterpolatedStringMiddleTokenType, "", this.laToken);
                break;
            case PythonLexer.RBRACE:
                // only when the previous brace expression is not a dictionary comprehension or set comprehension
                if (!IsValid_DictionaryOrSet_ComprehensionExpression(this.lastClosedBraceExpression))
                {
                    this.CreateAndAddPendingToken(this.activeInterpolatedStringMiddleTokenType, "", this.laToken);
                }
                break;
        }
    }

    // Determines whether the collected brace expression forms a dictionary or set
    // comprehension. Used to enforce Python’s rule that outermost f-string brace
    // expressions cannot be comprehensions.
    private static bool IsValid_DictionaryOrSet_ComprehensionExpression(string code)
    {
        var inputStream = CharStreams.fromString(code);
        var lexer = new PythonLexer(inputStream);
        var tokenStream = new CommonTokenStream(lexer);
        var parser = new PythonParser(tokenStream);

        // Disable error listeners to suppress console output
        lexer.RemoveErrorListeners();
        parser.RemoveErrorListeners();

        parser.dictcomp(); // Try parsing as dictionary comprehension
        if (parser.NumberOfSyntaxErrors == 0)
            return true;

        parser = new PythonParser(tokenStream);
        tokenStream.Seek(0);
        parser.RemoveErrorListeners();
        parser.setcomp(); // Try parsing as set comprehension
        return parser.NumberOfSyntaxErrors == 0;
    }

    // ===================== Trailing‑Token Finalization =====================
    // Handles end-of-input cleanup, including emission of remaining DEDENT tokens,
    // final NEWLINE normalization, and generation of the EOF token to properly
    // terminate the logical token stream.

    private void InsertTrailingTokens()
    {
        switch (this.lastPendingTokenTypeFromDefaultChannel)
        {
            case PythonLexer.NEWLINE:
            case PythonLexer.DEDENT:
                break; // no trailing NEWLINE token is needed
            default:
                // insert an extra trailing NEWLINE token that serves as the end of the last statement
                this.CreateAndAddPendingToken(PythonLexer.NEWLINE, null, this.laToken); // laToken is EOF
                break;
        }
        this.InsertIndentOrDedentToken(0); // Now insert as many trailing DEDENT tokens as needed
    }

    private void HandleEOFtoken()
    {
        if (this.lastPendingTokenTypeFromDefaultChannel > 0)
        {  // there was a statement in the intStream (leading NEWLINE tokens are hidden)
            this.InsertTrailingTokens();
        }
        this.AddPendingToken(this.curToken);
    }

    // ===================== Pending‑Token Management =====================
    // Manages the queue of pending tokens, including emission of normal and hidden
    // tokens, preserving correct output order for the token stream.

    private void HideAndAddPendingToken(IToken originalToken)
    {
        var token = new CommonToken(originalToken);
        token.Channel = TokenConstants.HiddenChannel;
        this.AddPendingToken(token);
    }

    private void CreateAndAddPendingToken(int ttype, string? text, IToken originalToken)
    {
        var token = new CommonToken(originalToken);
        token.Type = ttype;
        token.Channel = TokenConstants.DefaultChannel;
        token.StopIndex = originalToken.StartIndex - 1;
        token.Text = text ?? "<" + this.Vocabulary.GetSymbolicName(ttype) + ">";

        this.AddPendingToken(token);
    }

    private void AddPendingToken(IToken token)
    {
        // save the last pending token type because the pendingTokenQueue can be empty by the nextToken()
        this.previousPendingTokenType = token.Type;
        if (token.Channel == TokenConstants.DefaultChannel)
        {
            this.lastPendingTokenTypeFromDefaultChannel = this.previousPendingTokenType;
        }
        this.pendingTokenQueue.AddLast(token);
    }

    // ===================== Error Reporting & Diagnostics =====================
    // Provides consistent lexer-level error reporting, including generation of
    // ERRORTOKEN instances, construction of human-readable diagnostic messages,
    // and insertion of error markers into the token stream to ensure that the
    // parser receives accurate context for recovery.

    private void ReportLexerError(string errMsg)
    {
        this.ErrorListenerDispatch.SyntaxError(this.ErrorOutput, this, this.curToken.Type, this.curToken.Line, this.curToken.Column, " LEXER" + PythonLexerBase.ERR_TXT + errMsg, null);
    }

    private void ReportError(string errMsg)
    {
        this.ReportLexerError(errMsg);
        this.CreateAndAddPendingToken(PythonLexer.ERRORTOKEN, PythonLexerBase.ERR_TXT + errMsg, this.laToken);
        // the ERRORTOKEN also triggers a parser error
    }
}
