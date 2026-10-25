using System;
using System.IO;
using Antlr4.Runtime;

public abstract class LexerBase : Lexer
{
    protected LexerBase(ICharStream input)
        : base(input, Console.Out, Console.Error)
    {
    }

    protected LexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
            : base(input, output, errorOutput)
    {
    }

    public bool Check1()
    {
        return InputStream.LA(1) != ':';
    }
}
