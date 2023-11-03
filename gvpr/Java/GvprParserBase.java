import org.antlr.v4.runtime.*;

public abstract class GvprParserBase extends Parser
{
    protected GvprParserBase(TokenStream input)
    {
        super(input);
    }

    protected boolean IsSemiRequired()
    {
	var c = this._input.LT(-1);
        var d = this._input.LT(1);
        return c.getType() != gvprParser.CCBC;
    }

    protected boolean IsSemiNotRequired()
    {
	var c = this._input.LT(-1);
	var d = this._input.LT(1);
        return c.getType() == gvprParser.CCBC;
    }
}
