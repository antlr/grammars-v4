using System;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;

public class PlSqlLexerBase : Lexer
{
    ICharStream myinput;

    public override string[] RuleNames => throw new NotImplementedException();

    public override IVocabulary Vocabulary => throw new NotImplementedException();

    public override string GrammarFileName => throw new NotImplementedException();

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

    public bool IsNewlineAtPos(int pos)
    {
        int la = myinput.LA(pos);
        return la == -1 || la == '\n';
    }

    public bool IsQQuoteDelimiter()
    {
        int openingOffset = TokenStartCharIndex + 2 - myinput.Index;
        int opening = myinput.LA(openingOffset);
        int closing = myinput.LA(-2);
        if (Char.IsHighSurrogate((char)opening)) {
            int openingLow = myinput.LA(openingOffset + 1);
            if (Char.IsLowSurrogate((char)openingLow)) {
                opening = Char.ConvertToUtf32((char)opening, (char)openingLow);
            }
        }
        if (Char.IsLowSurrogate((char)closing)) {
            int closingHigh = myinput.LA(-3);
            if (Char.IsHighSurrogate((char)closingHigh)) {
                closing = Char.ConvertToUtf32((char)closingHigh, (char)closing);
            }
        }
        switch (opening) {
            case '[': return closing == ']';
            case '{': return closing == '}';
            case '<': return closing == '>';
            case '(': return closing == ')';
            default: return closing == opening;
        }
    }
}
