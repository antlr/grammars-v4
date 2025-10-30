using Antlr4.Runtime;
using System.IO;
using System.Linq;

public abstract class JavaParserBase : Parser {
    private readonly ITokenStream _input;

    protected JavaParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
            : base(input, output, errorOutput) {
        _input = input;
    }

    public bool DoLastRecordComponent()
    {
        var ctx = this.Context;
        var tctx = ctx as JavaParser.RecordComponentListContext;
        var rcs = tctx.recordComponent();
        if (! rcs.Any()) return true;
        var count = rcs.Count();
        for (int c = 0; c < count; ++c)
        {
            var rc = rcs[c];
            if (rc.ELLIPSIS() != null && c+1 < count)
                return false;
        }
        return true;
    }

    public bool IsNotIdentifierAssign()
    {
	    var la = this.TokenStream.LA(1);
	    // If not identifier, return true because it can't be
	    // "identifier = ..."
	    switch (la) {
		    case JavaParser.IDENTIFIER:
		    case JavaParser.MODULE:
		    case JavaParser.OPEN:
		    case JavaParser.REQUIRES:
		    case JavaParser.EXPORTS:
		    case JavaParser.OPENS:
		    case JavaParser.TO:
		    case JavaParser.USES:
		    case JavaParser.PROVIDES:
		    case JavaParser.WHEN:
		    case JavaParser.WITH:
		    case JavaParser.TRANSITIVE:
		    case JavaParser.YIELD:
		    case JavaParser.SEALED:
		    case JavaParser.PERMITS:
		    case JavaParser.RECORD:
		    case JavaParser.VAR:
			    break;
		    default:
			    return true;
	    }
	    var la2 = this.TokenStream.LA(2);
	    if (la2 != JavaParser.ASSIGN) return true;
	    return false;
	    
    }
}
