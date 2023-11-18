using System;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;

public class GvprLexerBase : Lexer
{
    protected GvprLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    public GvprLexerBase(ICharStream input)
        : base(input)
    {
    }

    public override string[] RuleNames => throw new NotImplementedException();

    public override IVocabulary Vocabulary => throw new NotImplementedException();

    public override string GrammarFileName => throw new NotImplementedException();

    public bool IsColumnZero()
    {
        return this.Column == 1;
    }
}
