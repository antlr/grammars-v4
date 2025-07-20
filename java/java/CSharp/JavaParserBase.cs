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
		if (rcs.Any()) return true;
		var count = rcs.Count();
		for (int c = 0; c < count; ++c)
		{
			var rc = rcs[c];
			if (rc.ELLIPSIS() != null && c+1 < count)
				return false;
		}
		return true;
	}
}