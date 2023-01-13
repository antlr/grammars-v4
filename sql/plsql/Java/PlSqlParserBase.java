import org.antlr.v4.runtime.*;

public abstract class PlSqlParserBase extends Parser
{
    private boolean _isVersion12 = true;
    private boolean _isVersion10 = true;
    public PlSqlParserBase self;

    public PlSqlParserBase(TokenStream input) {
        super(input);
        self = this;
    }

    public boolean isVersion12() {
        return _isVersion12;
    }

    public void setVersion12(boolean value) {
        _isVersion12 = value;
    }

    public boolean isVersion10() {
        return _isVersion10;
    }

    public void setVersion10(boolean value) {
        _isVersion10 = value;
    }
}
