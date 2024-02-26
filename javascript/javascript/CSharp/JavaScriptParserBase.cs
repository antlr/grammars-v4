using Antlr4.Runtime;
using System.Collections.Generic;
using System.IO;
using static JavaScriptParser;

/// <summary>
/// All parser methods that used in grammar (p, prev, notLineTerminator, etc.)
/// should start with lower case char similar to parser rules.
/// </summary>
public abstract class JavaScriptParserBase : Parser
{
    private readonly Stack<string> _tagNames = new Stack<string>();
    public JavaScriptParserBase(ITokenStream input)
        : base(input)
    {
    }

    public JavaScriptParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput) : this(input)
    {
    }

    /// <summary>
    /// Short form for prev(String str)
    /// </summary>
    protected bool p(string str)
    {
        return prev(str);
    }

    /// <summary>
    /// Whether the previous token value equals to str
    /// </summary>
    protected bool prev(string str)
    {
        return ((ITokenStream)this.InputStream).LT(-1).Text.Equals(str);
    }

    // Short form for next(String str)
    protected bool n(string str)
    {
        return next(str);
    }

    // Whether the next token value equals to @param str
    protected bool next(string str)
    {
        return ((ITokenStream)this.InputStream).LT(1).Text.Equals(str);
    }

    protected bool notLineTerminator()
    {
        return !here(LineTerminator);
    }

    protected bool notOpenBraceAndNotFunction()
    {
        int nextTokenType = ((ITokenStream)this.InputStream).LT(1).Type;
        return nextTokenType != OpenBrace && nextTokenType != Function_;
    }

    protected bool closeBrace()
    {
        return ((ITokenStream)this.InputStream).LT(1).Type == CloseBrace;
    }

    /// <summary>Returns true if on the current index of the parser's
    /// token stream a token of the given type exists on the
    /// Hidden channel.
    /// </summary>
    /// <param name="type">
    /// The type of the token on the Hidden channel to check.
    /// </param>
    protected bool here(int type)
    {
        // Get the most recently emitted token.
        IToken currentToken = ((ITokenStream)this.InputStream).LT(-1);

        // Get the next token index.
        int nextTokenIndex = currentToken == null ? 0 : currentToken.TokenIndex + 1;

        // Get the token after the `currentToken`. By using `_input.get(index)`,
        // we also grab a token that is (possibly) on the HIDDEN channel.
        IToken nextToken = ((ITokenStream)this.InputStream).Get(nextTokenIndex);

        // Check if the token resides on the HIDDEN channel and if it's of the
        // provided type.
        return (nextToken.Channel == Lexer.Hidden) && (nextToken.Type == type);
    }

    /// <summary>
    /// Returns true if on the current index of the parser's
    /// token stream a token exists on the Hidden channel which
    /// either is a line terminator, or is a multi line comment that
    /// contains a line terminator.
    /// </summary>
    protected bool lineTerminatorAhead()
    {
        // Get the token ahead of the current index.
        int possibleIndexEosToken = CurrentToken.TokenIndex - 1;
        if (possibleIndexEosToken < 0) return false;
        IToken ahead = ((ITokenStream)this.InputStream).Get(possibleIndexEosToken);

        if (ahead.Channel != Lexer.Hidden)
        {
            // We're only interested in tokens on the Hidden channel.
            return false;
        }

        if (ahead.Type == LineTerminator)
        {
            // There is definitely a line terminator ahead.
            return true;
        }

        if (ahead.Type == WhiteSpaces)
        {
            // Get the token ahead of the current whitespaces.
            possibleIndexEosToken = CurrentToken.TokenIndex - 2;
            if (possibleIndexEosToken < 0) return false;
            ahead = ((ITokenStream)this.InputStream).Get(possibleIndexEosToken);
        }

        // Get the token's text and type.
        string text = ahead.Text;
        int type = ahead.Type;

        // Check if the token is, or contains a line terminator.
        return (type == MultiLineComment && (text.Contains("\r") || text.Contains("\n"))) ||
                (type == LineTerminator);
    }

    protected void pushHtmlTagName(string tagName)
    {
        _tagNames.Push(tagName);
    }

    protected bool popHtmlTagName(string tagName)
    {
        return string.Equals(_tagNames.Pop(),tagName, System.StringComparison.InvariantCulture);
    }
}
