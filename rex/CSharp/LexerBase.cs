using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Antlr4.Runtime;

public abstract class LexerBase : Lexer
{
    private ICharStream _input;
    protected LexerBase(ICharStream input)
        : base(input, Console.Out, Console.Error)
    {
        _input = input;
    }

    protected LexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
            : base(input, output, errorOutput)
    {
        _input = input;
    }

    public bool Check1()
    {
        return _input.LA(1) != ':';
    }
}

