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

#nullable enable
using Antlr4.Runtime;
using System;
using System.Text;
using System.Text.RegularExpressions;
using System.IO;
using System.Collections.Generic;

public abstract class PythonLexerBase : Lexer
{
    // A stack that keeps track of the indentation lengths
    private Stack<int> indentLengthStack = new();
    // A list where tokens are waiting to be loaded into the token stream
    private LinkedList<IToken> pendingTokens = new();

    // last pending token type
    private int previousPendingTokenType;
    private int lastPendingTokenTypeFromDefaultChannel;

    // The amount of opened parentheses, square brackets, or curly braces
    private int opened;
    // The amount of opened parentheses and square brackets in the current lexer mode
    private Stack<int> paren_or_bracket_openedStack = new();
    // A stack that stores expression(s) between braces in fstring
    private Stack<string> braceExpressionStack = new();
    private string prevBraceExpression = "";

    // Instead of this._mode      (_mode is not implemented in each ANTLR4 runtime)
    private int curLexerMode;
    // Instead of this._modeStack (_modeStack is not implemented in each ANTLR4 runtime)
    private Stack<int> lexerModeStack = new();

    private bool wasSpaceIndentation;
    private bool wasTabIndentation;
    private bool wasIndentationMixedWithSpacesAndTabs;

    private IToken curToken = null!; // current (under processing) token
    private IToken ffgToken = null!; // following (look ahead) token

    private const int INVALID_LENGTH = -1;
    private const string ERR_TXT = " ERROR: ";

    protected PythonLexerBase(ICharStream input) : base(input)
    {
    }

    protected PythonLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput) : base(input, output, errorOutput)
    {
    }

    public override IToken NextToken() // reading the intStream stream until a return EOF
    {
        this.CheckNextToken();
        IToken firstPendingToken = this.pendingTokens.First!.Value;
        this.pendingTokens.RemoveFirst();
        return firstPendingToken; // add the queued token to the token stream
    }

    public override void Reset()
    {
        this.Init();
        base.Reset();
    }

    private void Init()
    {
        this.indentLengthStack = new();
        this.pendingTokens = new();
        this.previousPendingTokenType = 0;
        this.lastPendingTokenTypeFromDefaultChannel = 0;
        this.opened = 0;
        this.paren_or_bracket_openedStack = new();
        this.braceExpressionStack = new();
        this.prevBraceExpression = "";
        this.curLexerMode = 0;
        this.lexerModeStack = new();
        this.wasSpaceIndentation = false;
        this.wasTabIndentation = false;
        this.wasIndentationMixedWithSpacesAndTabs = false;
        this.curToken = null!;
        this.ffgToken = null!;
    }

    private void CheckNextToken()
    {
        if (this.previousPendingTokenType == TokenConstants.EOF)
            return;

        if (this.indentLengthStack.Count == 0) // We're at the first token
        {
            this.InsertENCODINGtoken();
            this.SetCurrentAndFollowingTokens();
            this.HandleStartOfInput();
        }
        else
        {
            this.SetCurrentAndFollowingTokens();
        }


        switch (this.curToken.Type)
        {
            case PythonLexer.NEWLINE:
                this.HandleNEWLINEtoken();
                break;
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
            case PythonLexer.FSTRING_MIDDLE:
                this.HandleFSTRING_MIDDLEtokenWithDoubleBrace(); // does not affect the opened field
                this.AddPendingToken(this.curToken);
                break;
            case PythonLexer.COLONEQUAL:
                this.HandleCOLONEQUALtokenInFString();
                break;
            case PythonLexer.ERRORTOKEN:
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
        this.HandleFORMAT_SPECIFICATION_MODE();
    }

    private void SetCurrentAndFollowingTokens()
    {
        this.curToken = this.ffgToken == null ?
                        base.NextToken() :
                        this.ffgToken;

        this.CheckCurToken(); // ffgToken cannot be used in this method and its sub methods (ffgToken is not yet set)!

        this.ffgToken = this.curToken.Type == TokenConstants.EOF ?
                        this.curToken :
                        base.NextToken();
    }

    private void InsertENCODINGtoken() // https://peps.python.org/pep-0263/
    {
        var lineBuilder = new StringBuilder();
        var encodingName = "";
        var lineCount = 0;
        var ws_commentPattern = new Regex("^[ \t\f]*(#.*)?$");
        var intStream = this.InputStream;
        var size = intStream.Size;

        intStream.Seek(0);
        for (int i = 0; i < size; i++)
        {
            char c = (char)intStream.LA(i + 1);
            lineBuilder.Append(c);

            if (c == '\n' || i == size - 1)
            {
                string line = lineBuilder.ToString().Replace("\r", "").Replace("\n", "");
                if (ws_commentPattern.IsMatch(line)) // WS* + COMMENT? found
                {
                    encodingName = GetEncodingName(line);
                    if (encodingName != "")
                    {
                        break; // encoding found
                    }
                }
                else
                {
                    break; // statement or backslash found (line is not empty, not whitespace(s), not comment)
                }

                lineCount++;
                if (lineCount >= 2)
                {
                    break; // check only the first two lines
                }
                lineBuilder.Clear();
            }
        }

        if (encodingName == "")
        {
            encodingName = "utf-8"; // default Python source code encoding
        }

        var encodingToken = new CommonToken(PythonLexer.ENCODING, encodingName);
        encodingToken.Channel = TokenConstants.HiddenChannel;
        encodingToken.StartIndex = 0;
        encodingToken.StopIndex = 0;
        encodingToken.Line = 0;
        encodingToken.Column = -1;
        AddPendingToken(encodingToken);
    }

    private static string GetEncodingName(string commentText) // https://peps.python.org/pep-0263/#defining-the-encoding
    {
        var encodingCommentPattern = new Regex("^[ \\t\\f]*#.*?coding[:=][ \\t]*([-_.a-zA-Z0-9]+)");
        var match = encodingCommentPattern.Match(commentText);
        return match.Success ? match.Groups[1].Value : string.Empty;
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
            IToken prevToken = this.pendingTokens.Last!.Value;
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
        if (this.lexerModeStack.Count > 0)
        {
            this.AddPendingToken(this.curToken);
        }
        else if (this.opened > 0)
        {
            // We're in an implicit line joining, ignore the current NEWLINE token
            this.HideAndAddPendingToken(this.curToken);
        }
        else
        {
            IToken nlToken = new CommonToken(this.curToken); // save the current NEWLINE token
            bool isLookingAhead = this.ffgToken.Type == PythonLexer.WS;
            if (isLookingAhead)
            {
                this.SetCurrentAndFollowingTokens(); // set the next two tokens
            }

            switch (this.ffgToken.Type)
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

    private void CheckCurToken()
    {
        switch (this.curToken.Type)
        {
            case PythonLexer.FSTRING_START:
                this.SetLexerModeByFSTRING_STARTtoken();
                return;
            case PythonLexer.FSTRING_MIDDLE:
                this.HandleFSTRING_MIDDLEtokenWithQuoteAndLBrace(); // affect the opened field
                if (this.curToken.Type == PythonLexer.FSTRING_MIDDLE)
                    return; // No curToken exchange happened
                break;
            case PythonLexer.FSTRING_END:
                this.PopLexerMode();
                return;
            default:
                if (this.lexerModeStack.Count == 0)
                    return; // Not in fstring mode
                break;
        }

        switch (this.curToken.Type)
        {
            case PythonLexer.NEWLINE:
                // append the current brace expression with the current newline
                this.AppendToBraceExpression(this.curToken.Text);
                var ctkn = new CommonToken(this.curToken);
                ctkn.Channel = TokenConstants.HiddenChannel;
                this.curToken = ctkn;
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
                this.IncrementBraceStack();
                break;
            case PythonLexer.RPAR:
            case PythonLexer.RSQB:
                // append the current brace expression with a ")" or a "]"
                this.AppendToBraceExpression(this.curToken.Text);
                this.DecrementBraceStack();
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
        this.braceExpressionStack.Push(this.braceExpressionStack.Pop() + text);
    }

    private void IncrementBraceStack()
    { // increment the last element (peek() + 1)
        this.paren_or_bracket_openedStack.Push(this.paren_or_bracket_openedStack.Pop() + 1);
    }

    private void DecrementBraceStack()
    { // decrement the last element (peek() - 1)
        this.paren_or_bracket_openedStack.Push(this.paren_or_bracket_openedStack.Pop() - 1);
    }

    private void SetLexerModeAfterRBRACEtoken()
    {
        switch (this.curLexerMode)
        {
            case Lexer.DEFAULT_MODE:
                this.PopLexerMode();
                this.PopByBRACE();
                break;
            case PythonLexer.SQ1__FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ1R_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1__FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ1R_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3__FORMAT_SPECIFICATION_MODE:
            case PythonLexer.SQ3R_FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3__FORMAT_SPECIFICATION_MODE:
            case PythonLexer.DQ3R_FORMAT_SPECIFICATION_MODE:
                this.PopLexerMode();
                this.PopLexerMode();
                this.PopByBRACE();
                break;
            default:
                this.ReportLexerError("f-string: single '}' is not allowed");
                break;
        }
    }

    private void SetLexerModeByFSTRING_STARTtoken()
    {
        string text = this.curToken.Text.ToLower();
        var modeMap = new Dictionary<string, int>
    {
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
        { "fr\"\"\"", PythonLexer.DQ3R_FSTRING_MODE }
    };

        if (modeMap.TryGetValue(text, out int mode))
        {
            this.PushLexerMode(mode);
        }
    }

    private void SetLexerModeByCOLONorCOLONEQUALtoken()
    {
        if (this.paren_or_bracket_openedStack.Peek() == 0)
        {
            // COLONEQUAL token will be replaced with a COLON token in CheckNextToken()
            switch (this.lexerModeStack.Peek())
            { // check the previous lexer mode (the current is DEFAULT_MODE)
                case PythonLexer.SQ1__FSTRING_MODE:
                case PythonLexer.SQ1__FORMAT_SPECIFICATION_MODE:
                    this.PushLexerMode(PythonLexer.SQ1__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.SQ1R_FSTRING_MODE:
                case PythonLexer.SQ1R_FORMAT_SPECIFICATION_MODE:
                    this.PushLexerMode(PythonLexer.SQ1R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.DQ1__FSTRING_MODE:
                case PythonLexer.DQ1__FORMAT_SPECIFICATION_MODE:
                    this.PushLexerMode(PythonLexer.DQ1__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.DQ1R_FSTRING_MODE:
                case PythonLexer.DQ1R_FORMAT_SPECIFICATION_MODE:
                    this.PushLexerMode(PythonLexer.DQ1R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.SQ3__FSTRING_MODE:
                case PythonLexer.SQ3__FORMAT_SPECIFICATION_MODE:
                    this.PushLexerMode(PythonLexer.SQ3__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.SQ3R_FSTRING_MODE:
                case PythonLexer.SQ3R_FORMAT_SPECIFICATION_MODE:
                    this.PushLexerMode(PythonLexer.SQ3R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.DQ3__FSTRING_MODE:
                case PythonLexer.DQ3__FORMAT_SPECIFICATION_MODE:
                    this.PushLexerMode(PythonLexer.DQ3__FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
                case PythonLexer.DQ3R_FSTRING_MODE:
                case PythonLexer.DQ3R_FORMAT_SPECIFICATION_MODE:
                    this.PushLexerMode(PythonLexer.DQ3R_FORMAT_SPECIFICATION_MODE); // continue in format spec. mode
                    break;
            }
        }
    }

    private void PopByBRACE()
    {
        this.paren_or_bracket_openedStack.Pop();
        this.prevBraceExpression = this.braceExpressionStack.Pop() + "}";
        if (this.braceExpressionStack.Count > 0)
        {
            // append the current brace expression with the previous brace expression
            this.braceExpressionStack.Push(this.braceExpressionStack.Pop() + this.prevBraceExpression);
        }

    }

    private void HandleFSTRING_MIDDLEtokenWithDoubleBrace()
    {
        // replace the trailing double brace with a single brace and insert a hidden brace token
        switch (this.GetLastTwoCharsOfTheCurTokenText())
        {
            case "{{":
                this.TrimLastCharAddPendingTokenSetCurToken(PythonLexer.LBRACE, "{", TokenConstants.HiddenChannel);
                break;
            case "}}":
                this.TrimLastCharAddPendingTokenSetCurToken(PythonLexer.RBRACE, "}", TokenConstants.HiddenChannel);
                break;
        }
    }

    private void HandleFSTRING_MIDDLEtokenWithQuoteAndLBrace()
    {
        // replace the trailing     quote + left_brace with a quote     and insert an LBRACE token
        // replace the trailing backslash + left_brace with a backslash and insert an LBRACE token
        switch (this.GetLastTwoCharsOfTheCurTokenText())
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
        string curTokenText = this.curToken.Text;
        return curTokenText.Length >= 2 ? curTokenText.Substring(curTokenText.Length - 2) : curTokenText;
    }

    private void TrimLastCharAddPendingTokenSetCurToken(int type, string text, int channel)
    {
        // trim the last char and add the modified curToken to the pendingTokens stack
        string curTokenText = this.curToken.Text;
        string tokenTextWithoutLastChar = curTokenText.Substring(0, curTokenText.Length - 1);
        var ctkn = new CommonToken(this.curToken);
        ctkn.Text = tokenTextWithoutLastChar;
        ctkn.StopIndex = ctkn.StopIndex - 1;
        this.AddPendingToken(ctkn);

        this.CreateNewCurToken(type, text, channel); // set curToken
    }

    private void HandleCOLONEQUALtokenInFString()
    {
        if (this.lexerModeStack.Count > 0 &&
            this.paren_or_bracket_openedStack.Peek() == 0)
        {
            // In fstring a colonequal (walrus operator) can only be used in parentheses
            // Not in parentheses, replace COLONEQUAL token with COLON as format specifier
            // and insert the equal symbol to the following FSTRING_MIDDLE token
            var ctkn = new CommonToken(this.curToken);
            ctkn.Type = PythonLexer.COLON;
            ctkn.Text = ":";
            ctkn.StopIndex = ctkn.StartIndex;
            this.curToken = ctkn;
            if (this.ffgToken.Type == PythonLexer.FSTRING_MIDDLE)
            {
                ctkn = new CommonToken(this.ffgToken);
                ctkn.Text = "=" + ctkn.Text;
                ctkn.StartIndex -= 1;
                ctkn.Column -= 1;
                this.ffgToken = ctkn;
            }
            else
            {
                this.AddPendingToken(this.curToken);
                this.CreateNewCurToken(PythonLexer.FSTRING_MIDDLE, "=", TokenConstants.DefaultChannel);
            }
        }
        this.AddPendingToken(this.curToken);
    }

    private void CreateNewCurToken(int type, string text, int channel)
    {
        var ctkn = new CommonToken(this.curToken);
        ctkn.Type = type;
        ctkn.Text = text;
        ctkn.Channel = channel;
        ctkn.Column += 1;
        ctkn.StartIndex += 1;
        ctkn.StopIndex = ctkn.StartIndex;
        this.curToken = ctkn;
    }

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

    private void HandleFORMAT_SPECIFICATION_MODE()
    {
        if (this.lexerModeStack.Count > 0
         && this.ffgToken.Type == PythonLexer.RBRACE)
        {
            // insert an empty FSTRING_MIDDLE token instead of the missing format specification
            switch (this.curToken.Type)
            {
                case PythonLexer.COLON:
                    this.CreateAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, TokenConstants.DefaultChannel, "", this.ffgToken);
                    break;
                case PythonLexer.RBRACE:
                    // only if the previous brace expression is not a dictionary comprehension or set comprehension
                    if (!IsDictionaryComprehensionOrSetComprehension(this.prevBraceExpression))
                    {
                        this.CreateAndAddPendingToken(PythonLexer.FSTRING_MIDDLE, TokenConstants.DefaultChannel, "", this.ffgToken);
                    }
                    break;
            }
        }
    }

    private static bool IsDictionaryComprehensionOrSetComprehension(string code)
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
        {  // there was a statement in the intStream (leading NEWLINE tokens are hidden)
            this.InsertTrailingTokens();
        }
        this.AddPendingToken(this.curToken);
    }

    private void HideAndAddPendingToken(IToken tkn)
    {
        var ctkn = new CommonToken(tkn);
        ctkn.Channel = TokenConstants.HiddenChannel;
        this.AddPendingToken(ctkn);
    }

    private void CreateAndAddPendingToken(int ttype, int channel, string? text, IToken sampleToken)
    {
        var ctkn = new CommonToken(sampleToken);
        ctkn.Type = ttype;
        ctkn.Channel = channel;
        ctkn.StopIndex = sampleToken.StartIndex - 1;
        ctkn.Text = text ?? "<" + this.Vocabulary.GetSymbolicName(ttype) + ">";

        this.AddPendingToken(ctkn);
    }

    private void AddPendingToken(IToken tkn)
    {
        // save the last pending token type because the pendingTokens list can be empty by the nextToken()
        this.previousPendingTokenType = tkn.Type;
        if (tkn.Channel == TokenConstants.DefaultChannel)
        {
            this.lastPendingTokenTypeFromDefaultChannel = this.previousPendingTokenType;
        }
        this.pendingTokens.AddLast(tkn);
    }

    private int GetIndentationLength(string indentText) // the indentText may contain spaces, tabs or form feeds
    {
        const int TAB_LENGTH = 8; // the standard number of spaces to replace a tab with spaces
        int length = 0;
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
                length = PythonLexerBase.INVALID_LENGTH; // only for the first inconsistent indent
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

        // the ERRORTOKEN will raise an error in the parser
        this.CreateAndAddPendingToken(PythonLexer.ERRORTOKEN, TokenConstants.DefaultChannel, PythonLexerBase.ERR_TXT + errMsg, this.ffgToken);
    }
}
