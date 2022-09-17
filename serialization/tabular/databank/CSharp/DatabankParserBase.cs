using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class DatabankParserBase : Parser
{
    protected DatabankParserBase(ITokenStream input)
        : base(input)
    {
    }

    protected DatabankParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }


    protected bool isdatatype()
    {
        var text = ((ITokenStream)this.InputStream).LT(-1).Text;
        return text == "-1" || text == "-4" || text == "-12";
    }
}
