using System;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;

public class Fortran90LexerBase : Lexer
{
    protected Fortran90LexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    public Fortran90LexerBase(ICharStream input)
        : base(input)
    {
    }

    public override string[] RuleNames => throw new NotImplementedException();

    public override IVocabulary Vocabulary => throw new NotImplementedException();

    public override string GrammarFileName => throw new NotImplementedException();

    public bool IsColumnZero()
    {
        return this.Column == 0;
    }

    public bool VerifyNotOperator()
    {
        var c1 = this.InputStream.LA(1);
        if (c1 == 'a')
        {
            var c2 = this.InputStream.LA(2);
            if (c2 == 'n')
            {
                var c3 = this.InputStream.LA(3);
                if (c3 == 'd')
                {
                    var c4 = this.InputStream.LA(4);
                    if (c4 == '.')
                    {
                        return false;
                    }
                }
            }
        }
        else if (c1 == 'o')
        {
            var c2 = this.InputStream.LA(2);
            if (c2 == 'r')
            {
                var c3 = this.InputStream.LA(3);
                if (c3 == '.')
                {
                    return false;
                }
            }
        }
        return true;
    }
}
