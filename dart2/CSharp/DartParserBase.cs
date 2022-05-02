using System;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using System.Collections.Generic;
using System.Linq;

public abstract class DartParserBase : Parser
{
	public DartParserBase(ITokenStream input)
		: base(input)
	{
		asyncEtcAreKeywords.Push(false);
	}

	protected DartParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
	: base(input, output, errorOutput)
	{
		asyncEtcAreKeywords.Push(false);
	}

	// Enable the parser to treat AWAIT/YIELD as keywords in the body of an
	// `async`, `async*`, or `sync*` function. Access via methods below.
	public Stack<bool> asyncEtcAreKeywords = new Stack<bool>();

	// Use this to indicate that we are now entering an `async`, `async*`,
	// or `sync*` function.
	public void startAsyncFunction() { asyncEtcAreKeywords.Push(true); }

	// Use this to indicate that we are now entering a function which is
	// neither `async`, `async*`, nor `sync*`.
	public void startNonAsyncFunction() { asyncEtcAreKeywords.Push(false); }

	// Use this to indicate that we are now leaving any funciton.
	public void endFunction() { asyncEtcAreKeywords.Pop(); }

	// Whether we can recognize AWAIT/YIELD as an identifier/typeIdentifier.
	public bool asyncEtcPredicate(int tokenId)
	{
		if (tokenId == DartLexer.AWAIT || tokenId == DartLexer.YIELD) {
			return !asyncEtcAreKeywords.Peek();
		}
		return false;
	}

	public bool pred1()
	{
		return  asyncEtcPredicate(this.CurrentToken.Type);
	}
}
