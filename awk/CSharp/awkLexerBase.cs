using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class awkLexerBase : Lexer
{
    bool _afterExpr = false;

    public override Antlr4.Runtime.IToken NextToken()
    {
        var token = base.NextToken();
        if (token.Channel == 0) // DEFAULT_CHANNEL
        {
            this._afterExpr = token.Type == awkLexer.WORD
                     || token.Type == awkLexer.NUMBER
                     || token.Type == awkLexer.STRING
                     || token.Type == awkLexer.BUILTIN_FUNC_NAME
                     || token.Type == awkLexer.INCR
                     || token.Type == awkLexer.DECR
                     || token.Type == awkLexer.Rp
                     || token.Type == awkLexer.Rb;
        }
        return token;
    }

    public bool IsNotAfterExpr()
    {
	return ! this._afterExpr;
    }

    public awkLexerBase(ICharStream input)
        : base(input)
    {
    }

    public awkLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }
}

