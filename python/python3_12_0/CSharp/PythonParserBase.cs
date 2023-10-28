/*
 * Project      : a helper class to implement specific PEG grammar expressions in an ANTLR4 grammar
 *
 * Developed by : Robert Einhorn
 */

// Related PEG grammar expressions:
// &e
// https://peps.python.org/pep-0617/#e-3
//
// !e
// https://peps.python.org/pep-0617/#e-4

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

    public PythonParserBase self // for compatibility with PythonParserBase.py
    {
        get { return this; }
    }

    // https://docs.python.org/3/reference/lexical_analysis.html#soft-keywords
    public bool isEqualCurrentTokenText(string tokenText)
    {
        return CurrentToken.Text == tokenText;
    }

    public bool isnotEqualCurrentTokenText(string tokenText)
    {
        return !isEqualCurrentTokenText(tokenText); // for compatibility with the Python 'not' logical operator
    }
}
