using System;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;

public abstract class PlSqlParserBase : Parser
{
    private bool _isVersion10 = false;
    private bool _isVersion11 = true;
    private bool _isVersion12 = true;

    protected PlSqlParserBase(ITokenStream input)
        : base(input)
    {
    }

    public PlSqlParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput) : this(input)
    {
    }

    public bool isVersion10() => _isVersion10;

    public bool isVersion11() => _isVersion11;

    public bool isVersion12() => _isVersion12;

    public bool setVersion10(bool value) => _isVersion10 = value;

    public bool setVersion11(bool value) => _isVersion11 = value;

    public bool setVersion12(bool value) => _isVersion12 = value;

    public bool IsNotNumericFunction() {
	    var lt1 = (this.InputStream as CommonTokenStream).LT(1);
	    var lt2 = (this.InputStream as CommonTokenStream).LT(2);
	    if ( (lt1.Type == PlSqlParser.SUM ||
		lt1.Type == PlSqlParser.COUNT ||
		lt1.Type == PlSqlParser.AVG ||
		lt1.Type == PlSqlParser.MIN ||
		lt1.Type == PlSqlParser.MAX ||
		lt1.Type == PlSqlParser.ROUND ||
		lt1.Type == PlSqlParser.LEAST ||
		  lt1.Type == PlSqlParser.GREATEST) && lt2.Type == PlSqlParser.LEFT_PAREN)
		    return false;
	    return true;
    }
}
