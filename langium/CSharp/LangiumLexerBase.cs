using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public abstract class LangiumLexerBase : Lexer
{
	public LangiumLexerBase(ICharStream input)
			: base(input)
	{
	}

	public LangiumLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
			: base(input, output, errorOutput)
	{
	}

	public bool NoSlash() => this.InputStream.LA(1) != '/';
}
