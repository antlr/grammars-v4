using Antlr4.Runtime;

public abstract class GoBaseLexer : Lexer
{
    // The most recently produced token.
    private IToken lastToken;

    protected GoBaseLexer(ICharStream input) : base(input)
    {
    }

    public override IToken NextToken()
    {
        // Get the next token.
        IToken next = base.NextToken();

        if (next.Channel == DefaultTokenChannel) {
            // Keep track of the last token on the default channel.
            lastToken = next;
        }

        return next;
    }
}
