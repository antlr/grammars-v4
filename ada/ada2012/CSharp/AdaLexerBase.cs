using System;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;


public abstract class AdaLexerBase : Lexer
{
    private int _lastTokenType = 0;

    protected AdaLexerBase(ICharStream input) : base(input) { }

    protected AdaLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) { }

    public override IToken NextToken()
    {
        var token = base.NextToken();
        if (token.Channel == Lexer.DefaultTokenChannel)
        {
            _lastTokenType = token.Type;
        }
        return token;
    }

    protected bool IsCharLiteralAllowed()
    {
        // In Ada, a tick after an identifier, closing paren, or 'all' keyword
        // is an attribute tick, not the start of a character literal.
        return _lastTokenType != AdaLexer.IDENTIFIER_
            && _lastTokenType != AdaLexer.RP
            && _lastTokenType != AdaLexer.ALL;
    }
}
