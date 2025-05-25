using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using System.IO;

public abstract class BisonLexerBase : Lexer
{
    private readonly ICharStream _input;
    protected int percent_percent_count;

    public BisonLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
            : base(input, output, errorOutput)
    {
        _input = input;
    }


    public void NextMode()
    {
        ++percent_percent_count;
        if (percent_percent_count == 1)
        {
            return;
        } else if (percent_percent_count == 2)
        {
            this.PushMode(BisonLexer.EpilogueMode);
            return;
        } else
        {
            this.Type = BisonLexer.PercentPercent;
            return;
        }
    }

    public override void Reset()
    {
	    percent_percent_count = 0;
	    base.Reset();
    }
}

