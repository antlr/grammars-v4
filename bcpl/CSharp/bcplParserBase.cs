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
}