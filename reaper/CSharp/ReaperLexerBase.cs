using Antlr4.Runtime;
using System.Collections.Generic;
using System;
using System.IO;
using System.Text.RegularExpressions;

// based on GDScriptLexerBase.cs with modification
public abstract class ReaperLexerBase : Lexer {
	// A queue where extra tokens are pushed on (see the NEWLINE lexer rule).
	private LinkedList<IToken> Tokens = new LinkedList<IToken>();
	// The stack that keeps track of the indentation level.
	private Stack<int> Indents = new Stack<int>();

	public ReaperLexerBase(ICharStream input)
			: base(input)
	{
	}
	public ReaperLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
			: base(input, output, errorOutput)
	{
	}

	public override void Emit(IToken token)
	{
		base.Token = token;
		Tokens.AddLast(token);
	}

	private CommonToken CreateIndentToken(int type, string text)
	{
		int stop = CharIndex - 1;
		int start = text.Length == 0 ? stop : stop - text.Length + 1;
		return new CommonToken(Tuple.Create((ITokenSource)this, (ICharStream)InputStream), type, DefaultTokenChannel, start, stop);
	}

	public override IToken NextToken()
	{
		// Check if the end-of-file is ahead and there are still some DEDENTS expected.
		if (((ICharStream)InputStream).LA(1) == Eof && Indents.Count != 0)
		{
			// Remove any trailing EOF tokens from our buffer.
			for (var node = Tokens.First; node != null; )
			{
				var temp = node.Next;
				if (node.Value.Type == Eof)
				{
					Tokens.Remove(node);
				}
				node = temp;
			}

			// Now emit as many DEDENT tokens as needed.
			while (Indents.Count != 0)
			{
				Emit(CreateIndentToken(reaperParser.DEDENT, ""));
				Indents.Pop();
			}

			// Put the EOF back on the token stream.
			Emit(CreateIndentToken(Eof, "<EOF>"));
		}

		var next = base.NextToken();

		if (Tokens.Count == 0)
		{
			return next;
		}
		else
		{
			var x = Tokens.First.Value;
			Tokens.RemoveFirst();
			return x;
		}
	}

	// Calculates the indentation of the provided spaces, taking the
	// following rule into account:
	//
	// "Tabs are replaced (from left to right) by one to eight spaces
	//  such that the total number of characters up to and including
	//  the replacement is a multiple of eight [...]"
	//
	//  -- https://docs.python.org/3.1/reference/lexical_analysis.html#indentation
	private static int IndentWidth(string spaces)
	{
		int count = 0;
		foreach (char ch in spaces.ToCharArray())
		{
			count += ch == '\t' ? 8 - (count % 8) : 1;
		}
		return count;
	}

	// Called from the NEWLINE lexer rule's action. Reaper's grammar has no
	// use for the newline character itself as a parser token (statements
	// are juxtaposed, not newline-terminated); only the resulting
	// INDENT/DEDENT/SEP tokens matter.
	//
	// A bodyless class reference (the esolang wiki's "class-prototype",
	// e.g. the final bare "Cat" in the wiki's example program) is simply a
	// class_declaration with no following INDENT, which already
	// unambiguously distinguishes it from class_definition (which requires
	// INDENT destructor DEDENT) without needing a separate REDENT token.
	public void onNewline()
	{
		var spaces = (new Regex("[\r\n]+")).Replace(Text, "");
		int next = ((ICharStream)InputStream).LA(1);
		if (next == '\r' || next == '\n')
		{
			// A blank line: ignore it entirely.
			Skip();
			return;
		}
		if (next == Eof)
		{
			// Trailing newline right before end of file: flush any
			// outstanding DEDENTs so every open block is closed.
			while (Indents.Count != 0)
			{
				Emit(CreateIndentToken(reaperParser.DEDENT, ""));
				Indents.Pop();
			}
			Skip();
			return;
		}
		int indent = IndentWidth(spaces);
		int previous = Indents.Count == 0 ? 0 : Indents.Peek();
		if (indent == previous)
		{
			// Same indentation level: a plain statement separator.
			Emit(CreateIndentToken(reaperParser.SEP, ""));
		}
		else if (indent > previous)
		{
			Indents.Push(indent);
			Emit(CreateIndentToken(reaperParser.INDENT, spaces));
		}
		else
		{
			// Possibly emit more than one DEDENT token.
			while (Indents.Count != 0 && Indents.Peek() > indent)
			{
				Emit(CreateIndentToken(reaperParser.DEDENT, ""));
				Indents.Pop();
			}
			Skip();
		}
	}

	public override void Reset()
	{
		Tokens = new LinkedList<IToken>();
		Indents = new Stack<int>();
		base.Reset();
	}
}
