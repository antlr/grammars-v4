using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class GoParserBase : Parser
{
    protected GoParserBase(ITokenStream input)
        : base(input)
    {
    }

    protected GoParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }


    protected bool closingBracket()
    {
        int la = tokenStream.LA(1);
        return la == GoLexer.R_PAREN || la == GoLexer.R_CURLY;
    }

    private ITokenStream tokenStream
    {
        get
        {
            return TokenStream;
        }
    }
}
