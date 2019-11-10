using System;
using System.Collections.Generic;
using Antlr4.Runtime;
using static GoParseTree.GoLexer;

namespace GoParseTree
{
    public abstract class GoParserBase : Parser
    {
        protected GoParserBase(ITokenStream input) : base(input)
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
            int possibleIndexEosToken = CurrentToken.TokenIndex - 1;

            if (possibleIndexEosToken == -1)
            {
                return true;
            }

            IToken ahead = TokenStream.Get(possibleIndexEosToken);
            if (ahead.Channel != Lexer.Hidden)
            {
                // We're only interested in tokens on the HIDDEN channel.
                return false;
            }

            if (ahead.Type == TERMINATOR)
            {
                // There is definitely a line terminator ahead.
                return true;
            }

            if (ahead.Type == WS)
            {
                // Get the token ahead of the current whitespaces.
                possibleIndexEosToken = CurrentToken.TokenIndex - 2;

                if (possibleIndexEosToken == -1)
                {
                    return true;
                }

                ahead = TokenStream.Get(possibleIndexEosToken);
            }

            // Get the token's text and type.
            String text = ahead.Text;
            int type = ahead.Type;

            // Check if the token is, or contains a line terminator.
            return type == COMMENT && (text.Contains("\r") || text.Contains("\n")) ||
                   type == TERMINATOR;
        }

        /// <summary>
        /// Returns `true` if no line terminator exists between the specified
        /// token offset and the prior one on the `HIDDEN` channel.
        /// </summary>
        protected bool noTerminatorBetween(int tokenOffset)
        {
            BufferedTokenStream stream = (BufferedTokenStream) TokenStream;
            IList<IToken> tokens = stream.GetHiddenTokensToLeft(stream.LT(tokenOffset).TokenIndex);

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
            BufferedTokenStream stream = (BufferedTokenStream) TokenStream;
            int leftParams = 1;
            int rightParams = 0;

            if (stream.LT(tokenOffset).Type == L_PAREN)
            {
                // Scan past parameters
                while (leftParams != rightParams)
                {
                    tokenOffset++;
                    int tokenType = stream.LT(tokenOffset).Type;

                    if (tokenType == L_PAREN)
                    {
                        leftParams++;
                    }
                    else if (tokenType == R_PAREN)
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
            return TokenStream.LT(1).Text?.Equals(text) ?? false;
        }
    }
}