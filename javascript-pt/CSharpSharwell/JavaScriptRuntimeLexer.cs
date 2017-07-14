using Antlr4.Runtime;
using static PT.PM.JavaScriptParseTreeUst.Parser.JavaScriptParser;

public abstract class JavaScriptRuntimeLexer : Lexer
{
    // The most recently produced token.
    private IToken _lastToken = null;

    public JavaScriptRuntimeLexer(ICharStream input)
        : base(input)
    {
    }

    // A property indicating if the lexer should operate in strict mode.
    // When set to true, FutureReservedWords are tokenized, when false,
    // an octal literal can be tokenized.
    public bool strictMode { get; set; } = true;

    ///<summary>Return the next token from the character stream and records this last
    ///token in case it resides on the default channel. This recorded token
    ///is used to determine when the lexer could possibly match a regex
    ///literal.</summary>
    ///<returns>the next token from the character stream.</returns>
    public override IToken NextToken()
    {
        // Get the next token.
        IToken next = base.NextToken();

        if (next.Channel == Lexer.DefaultTokenChannel)
        {
            // Keep track of the last token on the default channel.
            _lastToken = next;
        }

        return next;
    }

    ///<summary>Returns <c>true</c> iff the lexer can match a regex literal.</summary>
    ///<returns><c>true</c> iff the lexer can match a regex literal.</returns>
    protected bool isRegexPossible()
    {
        if (_lastToken == null)
        {
            // No token has been produced yet: at the start of the input,
            // no division is possible, so a regex literal _is_ possible.
            return true;
        }

        switch (_lastToken.Type)
        {
            case Identifier:
            case NullLiteral:
            case BooleanLiteral:
            case This:
            case CloseBracket:
            case CloseParen:
            case OctalIntegerLiteral:
            case DecimalLiteral:
            case HexIntegerLiteral:
            case StringLiteral:
            case PlusPlus:
            case MinusMinus:
                // After any of the tokens above, no regex literal can follow.
                return false;
            default:
                // In all other cases, a regex literal _is_ possible.
                return true;
        }
    }
}