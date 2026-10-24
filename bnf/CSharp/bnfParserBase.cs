using Antlr4.Runtime;
using System.IO;

public abstract class bnfParserBase : Parser {
    private readonly ITokenStream _input;

    protected bnfParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
            : base(input, output, errorOutput)
    {
        _input = input;
    }

    public bool NotNL()
    {
        int i = 1;
        IToken c = (this.InputStream as CommonTokenStream).LT(i);
        var v = c.Type != bnfParser.NL;
        return v;
    }

    public bool NotAssign()
    {
        int i = 1;
        IToken c = (this.InputStream as CommonTokenStream).LT(i);
        while (c != null && c.Type == bnfLexer.WS)
            c = (this.InputStream as CommonTokenStream).LT(++i);
        var v = !(c.Type == bnfParser.ASSIGN1
             | c.Type == bnfParser.ASSIGN2
             | c.Type == bnfParser.ASSIGN3
             | c.Type == bnfParser.ASSIGN4);
        return v;
    }

    public bool NotLhs()
    {
        int i = 1;
        IToken c = (this.InputStream as CommonTokenStream).LT(i);
        while (c != null && c.Type == bnfLexer.WS)
            c = (this.InputStream as CommonTokenStream).LT(++i);
        if (c != null && c.Type != bnfLexer.X1)
            return true;
        // '<'
        for (;;)
        {
            while (c != null && c.Type == bnfLexer.WS)
                c = (this.InputStream as CommonTokenStream).LT(++i);
            if (c != null && c.Type != bnfLexer.ID && c.Type != bnfLexer.X2)
                return true;
            // ID
            if (c == null) return true;
            if (c.Type == bnfLexer.X2)
                break;
        }
        // '>'
        while (c != null && c.Type == bnfLexer.WS)
            c = (this.InputStream as CommonTokenStream).LT(++i);
        if (c.Type == bnfLexer.ASSIGN1) return false;
        if (c.Type == bnfLexer.ASSIGN2) return false;
        if (c.Type == bnfLexer.ASSIGN3) return false;
        if (c.Type == bnfLexer.ASSIGN4) return false;
        return true;
    }

    
}