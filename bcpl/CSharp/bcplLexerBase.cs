using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class bcplLexerBase : Lexer
{
	private int _stringLength = 0;

	public bcplLexerBase(ICharStream input)
			: base(input)
	{
	}

	public bcplLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
			: base(input, output, errorOutput)
	{
	}

	public void setStringLength() => this._stringLength = int.Parse(this.Text[..^1]);

	public bool consumeStringChars() => (--this._stringLength) > -1;
}
