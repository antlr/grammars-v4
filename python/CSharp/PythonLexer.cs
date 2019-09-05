using System.Collections.Generic;
using Antlr4.Runtime;
using PythonParseTree;

public abstract class PythonBaseLexer : Lexer
{
    public static int TabSize { get; set; } = 8;

    // The amount of opened braces, brackets and parenthesis.
    private int _opened;

    // The stack that keeps track of the indentation level.
    private readonly Stack<int> _indents = new Stack<int>();

    // A list where extra tokens are pushed on (see the NEWLINE lexer rule).
    private int _tokensInd;
    private readonly List<IToken> _tokens = new List<IToken>();

    protected PythonBaseLexer(ICharStream charStream)
            : base(charStream)
    {
    }

    public override void Emit(IToken token)
    {
        base.Emit(token);
        _tokens.Add(token);
    }

    public override IToken NextToken()
    {
        // Check if the end-of-file is ahead and there are still some DEDENTS expected.
        if (_input.La(1) == Eof && _indents.Count > 0)
        {
            // Remove any trailing EOF tokens from our buffer.
            while (_tokensInd < _tokens.Count && _tokens[_tokensInd].Type == Eof)
            {
                _tokens[_tokensInd] = null;
                _tokensInd++;
            }

            if (_tokensInd < _tokens.Count && _tokens[_tokensInd].Type != PythonLexer.LINE_BREAK)
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

            // Put the EOF back on the token stream.
            Emit(Eof);
        }

        IToken next = base.NextToken();

        if (_tokens.Count == _tokensInd)
        {
            return next;
        }

        var result = _tokens[_tokensInd];
        _tokens[_tokensInd++] = null;
        return result;
    }

    protected void HandleNewLine()
    {
        char next = (char)_input.La(1);

        Emit(new CommonToken(_tokenFactorySourcePair, PythonLexer.NEWLINE, Hidden,
            CharIndex - Text.Length, CharIndex)
        {
            Line = Line - 1,
            Text = Text
        });

        if (_opened == 0 && !IsNewLine(next) && next != '#')
        {
            Emit(PythonLexer.LINE_BREAK);

            int wsInd = 0;
            if (wsInd < Text.Length && IsNewLine(Text[wsInd]))
            {
                wsInd++;
                if (wsInd < Text.Length && IsNewLine(Text[wsInd]))
                    wsInd++;
            }

            int indent = GetIndentationCount(Text, wsInd);
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
    }

    protected bool AtStartOfInputWithSpaces()
    {
        int index = -1;

        while (char.IsWhiteSpace((char)_input.La(index)))
        {
            index--;
        }

        return _input.Index + index < 0; // TODO: check
    }

    protected void IncIndentLevel() => _opened++;

    protected void DecIndentLevel()
    {
        if (_opened > 0)
        {
            --_opened;
        }
    }

    private static bool IsNewLine(char c) => c == '\r' || c == '\n' || c == '\f';

    private void Emit(int tokenType)
    {
        Emit(new CommonToken(_tokenFactorySourcePair, tokenType, DefaultTokenChannel,
            CharIndex, CharIndex)
        {
            Line = Line,
            Column = Column,
            Text = ""
        });
    }

    // Calculates the indentation of the provided spaces, taking the
    // following rules into account:
    //
    // "Tabs are replaced (from left to right) by one to eight spaces
    //  such that the total number of characters up to and including
    //  the replacement is a multiple of eight [...]"
    //
    //  -- https://docs.python.org/3.1/reference/lexical_analysis.html#indentation
    private static int GetIndentationCount(string spaces, int startInd)
    {
        int count = 0;

        for (int i = startInd; i < spaces.Length; i++)
        {
            count += spaces[i] == '\t' ? TabSize - count % TabSize : 1;
        }

        return count;
    }
}