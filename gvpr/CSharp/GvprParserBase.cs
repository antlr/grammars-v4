using Antlr4.Runtime;
using System.IO;

public abstract class GvprParserBase : Parser {
    private readonly ITokenStream _input;

    protected GvprParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        _input = input;
    }

    public bool IsSemiRequired()
    {
        var c = (this.InputStream as CommonTokenStream).LT(-1);
        var d = (this.InputStream as CommonTokenStream).LT(1);
        return c.Type != gvprParser.CCBC;
    }

    public bool IsSemiNotRequired()
    {
        var c = (this.InputStream as CommonTokenStream).LT(-1);
        var d = (this.InputStream as CommonTokenStream).LT(1);
        return c.Type == gvprParser.CCBC;
    }
}
