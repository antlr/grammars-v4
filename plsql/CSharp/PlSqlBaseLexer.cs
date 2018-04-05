using Antlr4.Runtime;

public abstract class PlSqlBaseLexer : Lexer
{
    public PlSqlBaseLexer(ICharStream input)
        : base(input)
    {
    }

    protected bool IsNewlineAtPos(int pos)
    {
        int la = _input.La(pos);
        return la == -1 || la == '\n';
    }
}