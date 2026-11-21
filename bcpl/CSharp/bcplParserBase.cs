using Antlr4.Runtime;
using System.IO;

public abstract class bcplParserBase : Parser {
    private readonly ITokenStream _input;

    protected bcplParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(new ChannelCommonTokenStream(input), output, errorOutput)
    {
        _input = input;
    }

    public bool IsNl()
    {
        var c = (this.InputStream as ChannelCommonTokenStream).LT(-1, 2);
        var d = (this.InputStream as ChannelCommonTokenStream).LT(1, 2);
        return c.Type == bcplParser.NL;
    }

    public bool IsNotNl()
    {
        var c = (this.InputStream as ChannelCommonTokenStream).LT(-1, 2);
        var d = (this.InputStream as ChannelCommonTokenStream).LT(1, 2);
        return d.Type != bcplParser.NL;
    }

    // Guards for the "(semi X)*" loops so that a trailing blank line (which
    // satisfies the zero-width `nl` alternative of `semi`) does not cause the
    // loop to commit to another iteration when there is no further X to parse.
    public bool IsDeclarationStart()
    {
        var la = this.InputStream.LA(1);
        return la == bcplParser.KLET || la == bcplParser.KMANIFEST || la == bcplParser.KGLOBAL || la == bcplParser.KSTATIC;
    }

    public bool IsNotCB()
    {
        var la = this.InputStream.LA(1);
        return la != bcplParser.CB;
    }
}