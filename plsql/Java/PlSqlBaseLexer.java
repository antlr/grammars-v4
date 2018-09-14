import org.antlr.v4.runtime.*;

public abstract class PlSqlBaseLexer extends Lexer
{
    public PlSqlBaseLexer(CharStream input) 
	{
		super(input);
    }

    protected boolean IsNewlineAtPos(int pos)
    {
        int la = _input.LA(pos);
        return la == -1 || la == '\n';
    }
}
