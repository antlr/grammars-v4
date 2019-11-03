package PlSqlParseTree;

import org.antlr.v4.runtime.*;

public abstract class PlSqlLexerBase extends Lexer
{
    public PlSqlLexerBase(CharStream input)
	{
		super(input);
    }

    protected boolean IsNewlineAtPos(int pos)
    {
        int la = _input.LA(pos);
        return la == -1 || la == '\n';
    }
}
