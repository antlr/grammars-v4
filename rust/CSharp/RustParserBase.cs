using Antlr4.Runtime;
using System.IO;

public abstract class RustParserBase : Parser {
    private readonly ITokenStream _input;

    protected RustParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) {
        _input = input;
    }

    public bool next(char expect) {
        return _input.LA(1) == expect;
    }
}