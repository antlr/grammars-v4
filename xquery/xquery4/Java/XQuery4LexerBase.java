import org.antlr.v4.runtime.*;

public abstract class XQuery4LexerBase extends Lexer
{
    protected XQuery4LexerBase(CharStream input)
    {
        super(input);
    }

    // Returns true when the next character is an NCName start character.
    // Used in the lexer predicate: '<' { IsNCNameStart() }? -> pushMode(IN_ELEMENT_TAG)
    protected boolean IsNCNameStart()
    {
        int c = _input.LA(1);
        return isNCNameStartChar(c);
    }

    private static boolean isNCNameStartChar(int c)
    {
        return (c >= 'A' && c <= 'Z')
            || c == '_'
            || (c >= 'a' && c <= 'z')
            || (c >= 0x00C0 && c <= 0x00D6)
            || (c >= 0x00D8 && c <= 0x00F6)
            || (c >= 0x00F8 && c <= 0x02FF)
            || (c >= 0x0370 && c <= 0x037D)
            || (c >= 0x037F && c <= 0x1FFF)
            || (c >= 0x200C && c <= 0x200D)
            || (c >= 0x2070 && c <= 0x218F)
            || (c >= 0x2C00 && c <= 0x2FEF)
            || (c >= 0x3001 && c <= 0xD7FF)
            || (c >= 0xF900 && c <= 0xFDCF)
            || (c >= 0xFDF0 && c <= 0xFFFD)
            || (c >= 0x10000 && c <= 0xEFFFF);
    }

    // Pops the mode stack when inside an embedded expression in element/attribute content.
    // Called by the CC token action: '}' { PopModeIfNeeded(); }
    // When in root DEFAULT_MODE (stack is empty), this is a no-op.
    protected void PopModeIfNeeded()
    {
        if (!_modeStack.isEmpty())
            popMode();
    }
}
