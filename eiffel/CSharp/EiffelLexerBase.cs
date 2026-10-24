using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class EiffelLexerBase : Lexer {

    private static string[] invalidFreeOps = {
	":=.", ":=+", ":=-", "<<-", "<<+", "<<>", "<<>>"
    };

    public EiffelLexerBase(ICharStream input) : base(input) { }

    public EiffelLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
		    : base(input, output, errorOutput)
    {
    }

    public bool IsFreeOperator() {
	bool res = true;
	var tk = this.Text;
	for(int i = 0; res && i < invalidFreeOps.Length; i++)
	    res = invalidFreeOps[i] != tk;
	return res;
    }
}

