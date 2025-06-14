using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Antlr4.Runtime;

public abstract class LuaParserBase : Parser
{
    const bool debug = false;

    protected LuaParserBase(ITokenStream input)
        : base(input)
    {
    }

    protected LuaParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    protected void myreset()
    {
	base.Reset();
    }

    public bool IsFunctionCall()
    {
        var la = this.TokenStream.LT(1);
	if (la.Type != LuaParser.NAME) return false;
	la = this.TokenStream.LT(2);
	if (la.Type == LuaParser.OP) return false;
	return true;
    }
}
