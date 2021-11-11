using System;
using Antlr4.Runtime;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;

public class PostgreSQLLexerBase : Lexer
{
    protected static Queue<string> tags = new Queue<string>();

    public PostgreSQLLexerBase(ICharStream input) : base(input)
    {
    }
    public PostgreSQLLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
    : base(input, output, errorOutput)
    {
    }


    public override string[] RuleNames => throw new NotImplementedException();

    public override IVocabulary Vocabulary => throw new NotImplementedException();

    public override string GrammarFileName => throw new NotImplementedException();

    public void pushTag() { tags.Enqueue( this.Text); }

    public bool isTag() { return this.Text.Equals(tags.Peek()); }

    public void popTag()
    {
        tags.Dequeue();
    }

    public void UnterminatedBlockCommentDebugAssert()
    {
        Debug.Assert(InputStream.LA(1) == -1 /*EOF*/);
    }
}
