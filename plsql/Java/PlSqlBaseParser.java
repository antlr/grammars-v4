import org.antlr.v4.runtime.*;

public abstract class PlSqlBaseParser extends Parser
{
    private boolean _isVersion12 = true;

    public PlSqlBaseParser(TokenStream input) {
        super(input);
    }

    public boolean isVersion12() {
        return _isVersion12;
    }

    public void setVersion12(boolean value) {
        _isVersion12 = value;
    }
}
