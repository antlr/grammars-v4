import org.antlr.v4.runtime.*;

abstract class GvprLexerBase extends Lexer
{
    protected GvprLexerBase(CharStream input)
    {
        super(input);
    }

    protected boolean IsColumnZero()
    {
	return this.getCharPositionInLine() == 1;
    }
}
