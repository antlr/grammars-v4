using Antlr4.Runtime;
using System.IO;

public abstract class CPP14ParserBase : Parser {

    protected CPP14ParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    public bool IsPureSpecifierAllowed()
    {
        try
        {
            var x = this.Context; // memberDeclarator
            var c = x.GetChild(0).GetChild(0);
            var c2 = c.GetChild(0);
            var p = c2.GetChild(1);
            if (p == null) return false;
            return p.GetType() == typeof(CPP14Parser.ParametersAndQualifiersContext);
        }
        catch
        {
        }
        return false;
    }
}
