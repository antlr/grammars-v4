using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class Dart2LexerBase : Lexer
{
	protected Dart2LexerBase(ICharStream input)
		: base(input)
	{
	}

	public Dart2LexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
		: base(input, output, errorOutput) { }

	protected bool CheckNotOpenBrace()
	{
		return this.InputStream.LA(1) != '{';
	}
}
