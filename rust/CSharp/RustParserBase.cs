using Antlr4.Runtime;
using System.IO;

public abstract class RustParserBase : Parser {
    private readonly ITokenStream _input;

    protected RustParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) {
        _input = input;
    }

    public bool NextGT() {
        return _input.LA(1) == RustParser.GT;
    }

    public bool NextLT() {
        return _input.LA(1) == RustParser.LT;
    }
}