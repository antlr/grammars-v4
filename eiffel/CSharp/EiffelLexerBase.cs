using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class EiffelLexerBase : Lexer {

    private static String[] invalidFreeOps = {
	":=.", ":=+", ":=-", "<<-", "<<+", "<<>", "<<>>"
    };

    public EiffelLexerBase(ICharStream input) : base(input) { }

    public EiffelLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
		    : base(input, output, errorOutput)
    {
    }

    public bool IsFreeOperator() {
	bool res = true;
	String tk = getText();
	for(int i = 0; res && i < invalidFreeOps.length; i++)
	    res = !invalidFreeOps[i].equals(tk);
	return res;
    }
}

