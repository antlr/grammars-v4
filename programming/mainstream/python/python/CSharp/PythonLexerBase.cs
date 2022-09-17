using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class PythonLexerBase : Lexer
{
    public static int TabSize { get; set; } = 8;

    // The amount of opened braces, brackets and parenthesis.
    private int _opened;

    // The stack that keeps track of the indentation level.
    private readonly Stack<int> _indents = new Stack<int>();

    // A circular buffer where extra tokens are pushed on (see the NEWLINE and WS lexer rules).
    private int _firstTokensInd;
    private int _lastTokenInd;
    private IToken[] _buffer = new IToken[32];
    private IToken _lastToken;

    protected PythonLexerBase(ICharStream charStream)
        : base(charStream)
    {
    }

    protected PythonLexerBase(ICharStream charStream, TextWriter output, TextWriter errorOutput)
        : base(charStream, output, errorOutput)
    {
    }

    public override void Emit(IToken token)
    {
        base.Emit(token);

        if (!(_buffer[_firstTokensInd] is null))
        {
            IncTokenInd(ref _lastTokenInd);

            if (_lastTokenInd == _firstTokensInd)
            {
                // Enlarge buffer
                var newArray = new IToken[_buffer.Length * 2];
                int destInd = newArray.Length - (_buffer.Length - _firstTokensInd);

                Array.Copy(_buffer, 0, newArray, 0, _firstTokensInd);
                Array.Copy(_buffer, _firstTokensInd, newArray, destInd, _buffer.Length - _firstTokensInd);

                _firstTokensInd = destInd;
                _buffer = newArray;
            }
        }

        _buffer[_lastTokenInd] = token;
        _lastToken = token;
    }

    public override IToken NextToken()
    {
        // Check if the end-of-file is ahead and there are still some DEDENTS expected.
        if (InputStream.LA(1) == Eof && _indents.Count > 0)
        {
            if (_buffer[_lastTokenInd] is null || _buffer[_lastTokenInd].Type != PythonLexer.LINE_BREAK)
            {
                // First emit an extra line break that serves as the end of the statement.
                Emit(PythonLexer.LINE_BREAK);
            }

            // Now emit as much DEDENT tokens as needed.
            while (_indents.Count != 0)
            {
                Emit(PythonLexer.DEDENT);
                _indents.Pop();
            }
        }

        IToken next = base.NextToken();

        if (_buffer[_firstTokensInd] is null)
        {
            return next;
        }

        var result = _buffer[_firstTokensInd];
        _buffer[_firstTokensInd] = null;

        if (_firstTokensInd != _lastTokenInd)
        {
            IncTokenInd(ref _firstTokensInd);
        }

        return result;
    }

    protected void HandleNewLine()
    {
        Emit(PythonLexer.NEWLINE, Hidden, Text);

        char next = (char) InputStream.LA(1);

        // Process whitespaces in HandleSpaces
        if (next != ' ' && next != '\t' && IsNotNewLineOrComment(next))
        {
            ProcessNewLine(0);
        }
    }

    protected void HandleSpaces()
    {
        char next = (char) InputStream.LA(1);

        if ((_lastToken == null || _lastToken.Type == PythonLexer.NEWLINE) && IsNotNewLineOrComment(next))
        {
            // Calculates the indentation of the provided spaces, taking the
            // following rules into account:
            //
            // "Tabs are replaced (from left to right) by one to eight spaces
            //  such that the total number of characters up to and including
            //  the replacement is a multiple of eight [...]"
            //
            //  -- https://docs.python.org/3.1/reference/lexical_analysis.html#indentation

            int indent = 0;

            foreach (char c in Text)
                indent += c == '\t' ? TabSize - indent % TabSize : 1;

            ProcessNewLine(indent);
        }

        Emit(PythonLexer.WS, Hidden, Text);
    }

    protected void IncIndentLevel() => _opened++;

    protected void DecIndentLevel()
    {
        if (_opened > 0)
        {
            --_opened;
        }
    }

    private bool IsNotNewLineOrComment(char next) =>
        _opened == 0 && next != '\r' && next != '\n' && next != '\f' && next != '#';

    private void ProcessNewLine(int indent)
    {
        Emit(PythonLexer.LINE_BREAK);

        int previous = _indents.Count == 0 ? 0 : _indents.Peek();

        if (indent > previous)
        {
            _indents.Push(indent);
            Emit(PythonLexer.INDENT);
        }
        else
        {
            // Possibly emit more than 1 DEDENT token.
            while (_indents.Count != 0 && _indents.Peek() > indent)
            {
                Emit(PythonLexer.DEDENT);
                _indents.Pop();
            }
        }
    }

    private void IncTokenInd(ref int ind) => ind = (ind + 1) % _buffer.Length;

    private void Emit(int tokenType, int channel = DefaultTokenChannel, string text = "")
    {
        IToken token =
#if LIGHT_TOKEN
            new PT.PM.AntlrUtils.LightToken((PT.PM.AntlrUtils.LightInputStream) _tokenFactorySourcePair.Item2,
                tokenType,
                channel, -1, CharIndex - text.Length, CharIndex);
#else
            new CommonToken(Tuple.Create((ITokenSource)this, (ICharStream)InputStream), tokenType, channel, CharIndex - text.Length, CharIndex - 1)
            {
                Line = Line,
                Column = Column,
                Text = text
            };
#endif
        Emit(token);
    }
}
