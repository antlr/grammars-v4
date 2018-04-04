using Antlr4.Runtime;

public abstract class PlSqlBaseLexer : Lexer
{
    private bool _isLineBegin = true;

    public PlSqlBaseLexer(ICharStream input)
        : base(input)
    {
    }

    public override IToken NextToken()
    {
        IToken next = base.NextToken();
        _isLineBegin = next.Text.EndsWith("\n");
        return next;
    }

    protected bool IsLineBegin() => _isLineBegin;
}