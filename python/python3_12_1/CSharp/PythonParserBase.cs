using Antlr4.Runtime;
using System.IO;

public abstract class PythonParserBase : Parser
{
    protected PythonParserBase(ITokenStream input) : base(input)
    {
    }

    protected PythonParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    // https://docs.python.org/3/reference/lexical_analysis.html#soft-keywords
    public bool isEqualToCurrentTokenText(string tokenText)
    {
        return CurrentToken.Text == tokenText;
    }

    public bool isnotEqualToCurrentTokenText(string tokenText)
    {
        return !isEqualToCurrentTokenText(tokenText); // for compatibility with the Python 'not' logical operator
    }
}
