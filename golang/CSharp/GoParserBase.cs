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

    /// <summary>
    /// Returns `true` if on the current index of the parser's
    /// token stream a token exists on the `HIDDEN` channel which
    /// either is a line terminator, or is a multi line comment that
    /// contains a line terminator.
    /// </summary>
    protected bool lineTerminatorAhead()
    {
        // Get the token ahead of the current index.
        int offset = 1;
        int possibleIndexEosToken = CurrentToken.TokenIndex - offset;

        if (possibleIndexEosToken == -1)
        {
            return true;
        }

        IToken ahead = tokenStream.Get(possibleIndexEosToken);

        while (ahead.Channel == Lexer.Hidden)
        {
            if (ahead.Type == GoLexer.TERMINATOR)
            {
                return true;
            }
            else if (ahead.Type == GoLexer.WS)
            {
                possibleIndexEosToken = this.CurrentToken.TokenIndex - ++offset;
                ahead = tokenStream.Get(possibleIndexEosToken);
            }
            else if (ahead.Type == GoLexer.COMMENT || ahead.Type == GoLexer.LINE_COMMENT )
            {
                if (ahead.Text.Contains("\r") || ahead.Text.Contains("\n"))
                {
                    return true;
                }
                else
                {
                    possibleIndexEosToken = this.CurrentToken.TokenIndex - ++offset;
                    ahead = tokenStream.Get(possibleIndexEosToken);
                }
            }
        }

        return false;
    }

    /// <summary>
    /// Returns `true` if no line terminator exists between the specified
    /// token offset and the prior one on the `HIDDEN` channel.
    /// </summary>
    protected bool noTerminatorBetween(int tokenOffset)
    {
        BufferedTokenStream stream = (BufferedTokenStream)tokenStream;
        IList<IToken> tokens = stream.GetHiddenTokensToLeft(LT(stream, tokenOffset).TokenIndex);

        if (tokens == null)
        {
            return true;
        }

        foreach (IToken token in tokens)
        {
            if (token.Text.Contains("\n"))
                return false;
        }

        return true;
    }

    /// <summary>
    /// Returns `true` if no line terminator exists after any encountered
    /// parameters beyond the specified token offset and the next on the
    /// `HIDDEN` channel.
    /// </summary>
    protected bool noTerminatorAfterParams(int tokenOffset)
    {
        BufferedTokenStream stream = (BufferedTokenStream) tokenStream;
        int leftParams = 1;
        int rightParams = 0;

        if (LT(stream, tokenOffset).Type == GoLexer.L_PAREN)
        {
            // Scan past parameters
            while (leftParams != rightParams)
            {
                tokenOffset++;
                int tokenType = LT(stream, tokenOffset).Type;

                if (tokenType == GoLexer.L_PAREN)
                {
                    leftParams++;
                }
                else if (tokenType == GoLexer.R_PAREN)
                {
                    rightParams++;
                }
            }

            tokenOffset++;
            return noTerminatorBetween(tokenOffset);
        }

        return true;
    }

    protected bool checkPreviousTokenText(string text)
    {
        return LT(tokenStream, 1).Text?.Equals(text) ?? false;
    }

    private IToken LT(ITokenStream stream, int k)
    {
        return stream.LT(k);
    }

    private ITokenStream tokenStream
    {
        get
        {
            return TokenStream;
        }
    }
}
