using System;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime;

    public abstract class PlSqlLexerBase : Lexer
    {
        ICharStream myinput;

	protected PlSqlLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
			: base(input, output, errorOutput)
	{
		myinput = input;
	}

        public PlSqlLexerBase(ICharStream input)
            : base(input)
	{
		myinput = input;
	}

        protected bool IsNewlineAtPos(int pos)
        {
		int la = myinput.LA(pos);
		return la == -1 || la == '\n';
        }
    }
