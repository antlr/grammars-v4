import org.antlr.v4.runtime.*;

public abstract class Fortran90LexerBase extends Lexer
{
    public Fortran90LexerBase(CharStream input)
    {
        super(input);
    }

    protected boolean IsColumnZero()
    {
	return this.getCharPositionInLine() == 0;
    }
}
