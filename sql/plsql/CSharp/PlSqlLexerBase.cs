using System;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;

public class PlSqlLexerBase : Lexer
{
    ICharStream myinput;
    public PlSqlLexerBase self;

    public override string[] RuleNames => throw new NotImplementedException();

    public override IVocabulary Vocabulary => throw new NotImplementedException();

    public override string GrammarFileName => throw new NotImplementedException();

    protected PlSqlLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        myinput = input;
        self = this;
    }

    public PlSqlLexerBase(ICharStream input)
        : base(input)
    {
        myinput = input;
        self = this;
    }

    public bool IsNewlineAtPos(int pos)
    {
        int la = myinput.LA(pos);
        return la == -1 || la == '\n';
    }
}
