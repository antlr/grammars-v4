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

    public boolean VerifyNotOperator()
    {
        var c1 = this.getInputStream().LA(1);
        if (c1 == 'a')
        {
            var c2 = this.getInputStream().LA(2);
            if (c2 == 'n')
            {
                var c3 = this.getInputStream().LA(3);
                if (c3 == 'd')
                {
                    var c4 = this.getInputStream().LA(4);
                    if (c4 == '.')
                    {
                        return false;
                    }
                }
            }
        }
        else if (c1 == 'o')
        {
            var c2 = this.getInputStream().LA(2);
            if (c2 == 'r')
            {
                var c3 = this.getInputStream().LA(3);
                if (c3 == '.')
                {
                    return false;
                }
            }
        }
        return true;
    }
}
