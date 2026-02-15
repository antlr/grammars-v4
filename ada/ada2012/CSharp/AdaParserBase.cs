using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class AdaParserBase : Parser
{
    protected AdaParserBase(ITokenStream input) : base(input) { }

    protected AdaParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) { }

    protected void ParsePragmas()
    {
        var stream = (BufferedTokenStream)TokenStream;
        stream.Fill();
        var allTokens = stream.GetTokens();
        const int PRAGMA_CHANNEL = 2;
        List<IToken> currentPragma = null;
        var pragmas = new List<List<IToken>>();
        foreach (var token in allTokens)
        {
            if (token.Channel != PRAGMA_CHANNEL)
                continue;
            if (token.Type == AdaLexer.PRAGMA)
            {
                currentPragma = new List<IToken>();
                currentPragma.Add(token);
            }
            else if (currentPragma != null)
            {
                currentPragma.Add(token);
                if (token.Type == AdaLexer.SEMI)
                {
                    pragmas.Add(currentPragma);
                    currentPragma = null;
                }
            }
        }
        foreach (var pragmaTokens in pragmas)
        {
            var defaultChannelTokens = new List<IToken>();
            foreach (var t in pragmaTokens)
            {
                var ct = new CommonToken(t);
                ct.Channel = Lexer.DefaultTokenChannel;
                defaultChannelTokens.Add(ct);
            }
            var eof = new CommonToken(TokenConstants.EOF);
            eof.Channel = Lexer.DefaultTokenChannel;
            defaultChannelTokens.Add(eof);
            var tokenSource = new ListTokenSource(defaultChannelTokens);
            var tokenStream = new CommonTokenStream(tokenSource);
            var parser = new AdaParser(tokenStream);
            parser.RemoveErrorListeners();
            foreach (var listener in this.ErrorListeners)
            {
                parser.AddErrorListener(listener);
            }
            parser.pragmaRule();
        }
    }
}
