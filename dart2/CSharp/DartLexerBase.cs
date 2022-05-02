using System;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using System.Collections.Generic;
using System.Linq;

public abstract class DartLexerBase : Lexer
{
	public DartLexerBase(ICharStream input) : base(input)
	{
	}

	public DartLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
	: base(input, output, errorOutput)
	{
	}

	int BRACE_NORMAL = 1;
	int BRACE_SINGLE = 2;
	int BRACE_DOUBLE = 3;
	int BRACE_THREE_SINGLE = 4;
	int BRACE_THREE_DOUBLE = 5;

	// Enable the parser to handle string interpolations via brace matching.
	// The top of the `braceLevels` stack describes the most recent unmatched
	// '{'. This is needed in order to enable/disable certain lexer rules.
	//
	//   NORMAL: Most recent unmatched '{' was not string literal related.
	//   SINGLE: Most recent unmatched '{' was `'...${`.
	//   DOUBLE: Most recent unmatched '{' was `"...${`.
	//   THREE_SINGLE: Most recent unmatched '{' was `'''...${`.
	//   THREE_DOUBLE: Most recent unmatched '{' was `"""...${`.
	//
	// Access via functions below.
	Stack<int> braceLevels = new Stack<int>();

	// Whether we are currently in a string literal context, and which one.
	public bool currentBraceLevel(int braceLevel)
	{
		if (!braceLevels.Any()) return false;
		return braceLevels.Peek() == braceLevel;
	}

	// Use this to indicate that we are now entering a specific '{...}'.
	// Call it after accepting the '{'.
	public void enterBrace() {
		braceLevels.Push(BRACE_NORMAL);
	}
	public void enterBraceSingleQuote() {
		braceLevels.Push(BRACE_SINGLE);
	}
	public void enterBraceDoubleQuote() {
		braceLevels.Push(BRACE_DOUBLE);
	}
	public void enterBraceThreeSingleQuotes() {
		braceLevels.Push(BRACE_THREE_SINGLE);
	}
	public void enterBraceThreeDoubleQuotes() {
		braceLevels.Push(BRACE_THREE_DOUBLE);
	}

	// Use this to indicate that we are now exiting a specific '{...}',
	// no matter which kind. Call it before accepting the '}'.
	public void exitBrace() {
		// We might raise a parse error here if the stack is empty, but the
		// parsing rules should ensure that we get a parse error anyway, and
		// it is not a big problem for the spec parser even if it misinterprets
		// the brace structure of some programs with syntax errors.
		if (braceLevels.Any()) braceLevels.Pop();
	}

	public void skip()
	{
		this.Skip();
	}

	public bool braceLevelSingle()
	{
		return currentBraceLevel(BRACE_SINGLE);
	}
	public bool braceLevelNormal()
	{
		return currentBraceLevel(BRACE_NORMAL);
	}

	public bool braceLevelDouble()
	{
		return currentBraceLevel(BRACE_DOUBLE);
	}

	public bool braceLevelTriple()
	{
		return currentBraceLevel(BRACE_THREE_DOUBLE);
	}
	public bool braceLevelTripleSingle()
	{
		return currentBraceLevel(BRACE_THREE_DOUBLE);
	}
}
