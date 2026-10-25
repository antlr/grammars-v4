#nullable enable
using Antlr4.Runtime;
using System.IO;

public abstract class Scala3ParserBase : Parser
{
    protected Scala3ParserBase(ITokenStream input) : base(input) { }
    protected Scala3ParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) { }

    /// <summary>
    /// Returns true when the --3.0-migration command-line flag is set,
    /// enabling Scala 2-compatible syntax (._  wildcard imports, [_] type wildcards).
    /// </summary>
    protected bool migration30() => Scala3LexerBase.Migration30;
}
