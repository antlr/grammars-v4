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

    public bool VerifyNotOperator()
    {
        var c1 = this.InputStream.LA(1);
        if (c1 == 'a')
        {
            var c2 = this.InputStream.LA(2);
            if (c2 == 'n')
            {
                var c3 = this.InputStream.LA(3);
                if (c3 == 'd')
                {
                    var c4 = this.InputStream.LA(4);
                    if (c4 == '.')
                    {
                        return false;
                    }
                }
            }
        }
        else if (c1 == 'o')
        {
            var c2 = this.InputStream.LA(2);
            if (c2 == 'r')
            {
                var c3 = this.InputStream.LA(3);
                if (c3 == '.')
                {
                    return false;
                }
            }
        }
        return true;
    }
}
