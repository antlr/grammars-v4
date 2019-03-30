using System.Collections.Generic;
using Antlr4.Runtime;
using System.Text.RegularExpressions;
using PythonParseTree;

public abstract class PythonBaseLexer : Lexer
{
    private static Regex _newLineRegex = new Regex("[^\r\n\f]+");
    private static Regex _spacesRegex = new Regex("[\r\n\f]+");

    // A queue where extra tokens are pushed on (see the NEWLINE lexer rule).
    private LinkedList<IToken> Tokens = new LinkedList<IToken>();
    // The stack that keeps track of the indentation level.
    private Stack<int> Indents = new Stack<int>();
    // The amount of opened braces, brackets and parenthesis.
    public int Opened = 0;
    // The most recently produced token.
    public IToken LastToken;

    public PythonBaseLexer(ICharStream charStream)
            : base(charStream)
    {
    }

    public override void Emit(IToken token)
    {
        base.Token = token;
        Tokens.AddLast(token);
    }

    public void HandleNewLine()
    {
        var newLine = _newLineRegex.Replace(Text, "");
        var spaces = _spacesRegex.Replace(Text, "");

        int next = _input.La(1);
        if (Opened > 0 || next == '\r' || next == '\n' || next == '\f' || next == '#')
        {
            // If we're inside a list or on a blank line, ignore all indents,
            // dedents and line breaks.
            Skip();
        }
        else
        {
            Emit(CommonToken(PythonLexer.NEWLINE, newLine));
            int indent = GetIndentationCount(spaces);
            int previous = Indents.Count == 0 ? 0 : Indents.Peek();
            if (indent == previous)
            {
                // skip indents of the same size as the present indent-size
                Skip();
            }
            else if (indent > previous)
            {
                Indents.Push(indent);
                Emit(CommonToken(PythonLexer.INDENT, spaces));
            }
            else
            {
                // Possibly emit more than 1 DEDENT token.
                while (Indents.Count != 0 && Indents.Peek() > indent)
                {
                    Emit(CreateDedent());
                    Indents.Pop();
                }
            }
        }
    }

    private CommonToken CommonToken(int type, string text)
    {
        int stop = CharIndex - 1;
        int start = text.Length == 0 ? stop : stop - text.Length + 1;
        return new CommonToken(_tokenFactorySourcePair, type, DefaultTokenChannel, start, stop);
    }

    public override IToken NextToken()
    {
        // Check if the end-of-file is ahead and there are still some DEDENTS expected.
        if (_input.La(1) == Eof && Indents.Count != 0)
        {
            // Remove any trailing EOF tokens from our buffer.
            for (var node = Tokens.First; node != null;)
            {
                var temp = node.Next;
                if (node.Value.Type == Eof)
                {
                    Tokens.Remove(node);
                }

                node = temp;
            }

            // First emit an extra line break that serves as the end of the statement.
            Emit(CommonToken(PythonLexer.NEWLINE, "\n"));

            // Now emit as much DEDENT tokens as needed.
            while (Indents.Count != 0)
            {
                Emit(CreateDedent());
                Indents.Pop();
            }

            // Put the EOF back on the token stream.
            Emit(CommonToken(Eof, "<EOF>"));
        }

        var next = base.NextToken();
        if (next.Channel == DefaultTokenChannel)
        {
            // Keep track of the last token on the default channel.
            LastToken = next;
        }

        if (Tokens.Count == 0)
        {
            return next;
        }

        var x = Tokens.First.Value;
        Tokens.RemoveFirst();
        return x;
    }

    public bool AtStartOfInputWithSpaces()
    {
        int index = -1;

        while (char.IsWhiteSpace((char)_input.La(index)))
        {
            index--;
        }

        return _input.Index + index < 0; // TODO: check
    }

    protected void IncIndentLevel()
    {
        Opened++;
    }

    protected void DecIndentLevel()
    {
        Opened--;
    }

    private IToken CreateDedent()
    {
        var dedent = CommonToken(PythonLexer.DEDENT, "");
        dedent.Line = LastToken.Line;
        return dedent;
    }

    // Calculates the indentation of the provided spaces, taking the
    // following rules into account:
    //
    // "Tabs are replaced (from left to right) by one to eight spaces
    //  such that the total number of characters up to and including
    //  the replacement is a multiple of eight [...]"
    //
    //  -- https://docs.python.org/3.1/reference/lexical_analysis.html#indentation
    private static int GetIndentationCount(string spaces)
    {
        int count = 0;

        foreach (char ch in spaces)
        {
            count += ch == '\t' ? 8 - count % 8 : 1;
        }

        return count;
    }
}