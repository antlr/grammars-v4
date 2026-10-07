using System;
using System.Globalization;
using System.IO;
using Antlr4.Runtime;

// STLexer.g4 names this superclass LexerAdaptor. The filename follows the
// existing StringTemplate port's LexAdaptor naming convention.
public abstract class LexerAdaptor : Lexer
{
    public char lDelim = '<';
    public char rDelim = '>';
    public int subtemplateDepth;

    protected LexerAdaptor(ICharStream input)
        : base(input, Console.Out, Console.Error) { }

    protected LexerAdaptor(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) { }

    // Predicates imported from LexBasic.g4 use Java's _input spelling.
    protected IIntStream _input => InputStream;

    public bool startsSubTemplate()
    {
        subtemplateDepth++;
        return true;
    }

    public bool endsSubTemplate()
    {
        if (subtemplateDepth > 0)
        {
            subtemplateDepth--;
            Mode(STLexer.Inside);
        }
        return true;
    }

    public void setDelimiters(char left, char right)
    {
        lDelim = left;
        rDelim = right;
    }

    // The wildcard in each lexer rule has already consumed the delimiter.
    public bool isLDelim() => InputStream.LA(-1) == lDelim;
    public bool isRDelim() => InputStream.LA(-1) == rDelim;
    public bool isLTmplComment() => isLDelim() && InputStream.LA(1) == '!';
    public bool isRTmplComment() => isRDelim() && InputStream.LA(-2) == '!';

    public bool adjText()
    {
        if (InputStream.LA(-1) == '\\')
        {
            int next = InputStream.LA(1);
            if (next == '\\' || next == lDelim || next == '}')
                InputStream.Consume();
        }
        return true;
    }
}

// STGLexer does not use LexerAdaptor, but imports the same LexBasic predicates.
public partial class STGLexer
{
    private IIntStream _input => InputStream;
}

// LexBasic.g4 embeds Java Character calls. Preserve the source grammar and
// provide the small compatibility surface needed by both generated lexers.
internal static class Character
{
    public static int toCodePoint(char high, char low) => char.ConvertToUtf32(high, low);

    public static bool isJavaIdentifierPart(int codePoint)
    {
        if (codePoint < 0 || codePoint > 0x10FFFF || codePoint is >= 0xD800 and <= 0xDFFF)
            return false;

        // Java permits identifier-ignorable controls in addition to the
        // Unicode letter, digit, mark, currency, connector, and format classes.
        if (codePoint is >= 0x00 and <= 0x08 or >= 0x0E and <= 0x1B
            or >= 0x7F and <= 0x9F)
            return true;

        return CharUnicodeInfo.GetUnicodeCategory(char.ConvertFromUtf32(codePoint), 0) switch
        {
            UnicodeCategory.UppercaseLetter or UnicodeCategory.LowercaseLetter
                or UnicodeCategory.TitlecaseLetter or UnicodeCategory.ModifierLetter
                or UnicodeCategory.OtherLetter or UnicodeCategory.LetterNumber
                or UnicodeCategory.NonSpacingMark or UnicodeCategory.SpacingCombiningMark
                or UnicodeCategory.EnclosingMark or UnicodeCategory.DecimalDigitNumber
                or UnicodeCategory.ConnectorPunctuation or UnicodeCategory.CurrencySymbol
                or UnicodeCategory.Format => true,
            _ => false
        };
    }
}
