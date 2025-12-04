using Antlr4.Runtime;
using System.IO;
using System;

public abstract class EiffelParserBase : Parser {

    protected EiffelParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    public bool IsNone()
    {
        return string.Equals("none", ((CommonTokenStream)InputStream).LT(2).Text, StringComparison.OrdinalIgnoreCase);
    }

    public bool IsTuple()
    {
        return string.Equals("tuple", ((CommonTokenStream)InputStream).LT(1).Text, StringComparison.OrdinalIgnoreCase);
    }
}