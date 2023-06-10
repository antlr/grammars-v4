import org.antlr.v4.runtime.*;

public abstract class Fortran77LexerBase extends Lexer
{
    public Fortran77LexerBase(CharStream input)
    {
        super(input);
    }

    protected boolean IsColumnZero()
    {
	return this.getCharPositionInLine() == 0;
    }
}
