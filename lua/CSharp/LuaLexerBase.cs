using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using Antlr4.Runtime;
using Microsoft.Build.Utilities;

public abstract class LuaLexerBase : Lexer
{
    public LuaLexerBase(ICharStream input)
            : base(input)
    {
    }

    public LuaLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
            : base(input, output, errorOutput)
    {
    }

    public void HandleComment()
    {
        var cs = (ICharStream)InputStream;
        if (cs.LA(1) == '[')
        {
            int sep = skip_sep(cs);
            if (sep >= 2)
            {
                read_long_string(cs, sep);
                return;
            }
        }
        while (cs.LA(1) != '\n' && cs.LA(1) != -1)
        {
            cs.Consume();
        }
    }

    private void read_long_string(ICharStream cs, int sep)
    {
        bool done = false;
        cs.Consume();
        for (; ; )
        {
            var c = cs.LA(1);
            var cc = (char)c;
            switch (c)
            {
                case -1:
                    done = true;
                    break;
                case ']':
                    if (skip_sep(cs) == sep) done = true;
                    break;
            }

            if (cs.LA(1) == -1) break;
            cs.Consume();
            if (done) break;
        }
    }

    private int skip_sep(ICharStream cs)
    {
        int count = 0;
        int s = cs.LA(1);
        char ss = (char)s;
        cs.Consume();
        while (cs.LA(1) == '=')
        {
            cs.Consume();
            count++;
        }
        if (cs.LA(1) == s) count += 2;
        else if (count == 0) count = 1;
        else count = 0;
        return count;
	}

	public bool IsLine1Col0()
	{
		var cs = (ICharStream)InputStream;
        if (cs.Index == 1) return true;
        return false;
    }
}
