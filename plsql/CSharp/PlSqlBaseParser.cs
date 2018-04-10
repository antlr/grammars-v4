using Antlr4.Runtime;

public abstract class PlSqlBaseParser : Parser
{
    private bool _isVersion12 = true;

    public PlSqlBaseParser(ITokenStream input)
        : base(input)
    {
    }

    public bool isVersion12() => _isVersion12;

    public bool setVersion12(bool value) => _isVersion12 = value;
}