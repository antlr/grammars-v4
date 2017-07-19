using Antlr4.Runtime;
using static PT.PM.JavaScriptParseTreeUst.Parser.JavaScriptParser;

public abstract class JavaScriptRuntimeParser : Parser
{
    public JavaScriptRuntimeParser(ITokenStream input)
        : base(input)
    {
    }

    protected bool p(string str) => prev(str);

    protected bool prev(string str)
    {
        return _input.Lt(-1).Text.Equals(str);
    }

    protected bool notLineTerminator()
    {
        return !here(LineTerminator);
    }

    protected bool notOpenBraceAndNotFunction()
    {
        return _input.Lt(1).Type != OpenBrace && _input.Lt(1).Type != Function;
    }

    protected bool closeBrace()
    {
        return _input.Lt(1).Type == CloseBrace;
    }

    ///<summary>Returns <c>true</c> iff on the current index of the parser's
    ///token stream a token of the given <c>type</c> exists on the
    ///<c>Hidden</c> channel.</summary>
    ///<param name="type">the type of the token on the <c>Hidden</c> channel
    ///to check.</param>
    ///<returns><c>true</c> iff on the current index of the parser's
    ///token stream a token of the given <c>type</c> exists on the
    ///<c>Hidden</c> channel.</returns>
    protected bool here(int type)
    {
        // Get the token ahead of the current index.
        int possibleIndexEosToken = CurrentToken.TokenIndex - 1;
        IToken ahead = _input.Get(possibleIndexEosToken);

        // Check if the token resides on the Hidden channel and if it's of the
        // provided type.
        return ahead.Channel == Lexer.Hidden && ahead.Type == type;
    }

    ///<summary>Returns <c>true</c> iff on the current index of the parser's
    ///token stream a token exists on the <c>Hidden</c> channel which
    ///either is a line terminator, or is a multi line comment that
    ///contains a line terminator.</summary>
    ///<returns><c>true</c> iff on the current index of the parser's
    ///token stream a token exists on the <c>Hidden</c> channel which
    ///either is a line terminator, or is a multi line comment that
    ///contains a line terminator.</returns>
    protected bool lineTerminatorAhead()
    {
        // Get the token ahead of the current index.
        int possibleIndexEosToken = CurrentToken.TokenIndex - 1;
        IToken ahead = _input.Get(possibleIndexEosToken);

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
            ahead = _input.Get(possibleIndexEosToken);
        }

        // Get the token's text and type.
        string text = ahead.Text;
        int type = ahead.Type;

        // Check if the token is, or contains a line terminator.
        return (type == MultiLineComment && (text.Contains("\r") || text.Contains("\n"))) ||
                (type == LineTerminator);
    }
}