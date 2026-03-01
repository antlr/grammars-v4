///{packageLine}

import org.antlr.v4.runtime.*;

public abstract class PlSqlParserBase extends Parser
{
    private boolean _isVersion12 = true;
    private boolean _isVersion11 = true;
    private boolean _isVersion10 = true;

    public PlSqlParserBase(TokenStream input) {
        super(input);
    }

    public boolean isVersion12() {
        return _isVersion12;
    }

    public void setVersion12(boolean value) {
        _isVersion12 = value;
    }

    public boolean isVersion11() {
        return _isVersion11;
    }

    public void setVersion11(boolean value) {
        _isVersion11 = value;
    }

    public boolean isVersion10() {
        return _isVersion10;
    }

    public void setVersion10(boolean value) {
        _isVersion10 = value;
    }

    public boolean IsNotNumericFunction() {
        Token lt1 = _input.LT(1);
        Token lt2 = _input.LT(2);
        if ((lt1.getType() == PlSqlParser.SUM ||
             lt1.getType() == PlSqlParser.COUNT ||
             lt1.getType() == PlSqlParser.AVG ||
             lt1.getType() == PlSqlParser.MIN ||
             lt1.getType() == PlSqlParser.MAX ||
             lt1.getType() == PlSqlParser.ROUND ||
             lt1.getType() == PlSqlParser.LEAST ||
             lt1.getType() == PlSqlParser.GREATEST) &&
             lt2.getType() == PlSqlParser.LEFT_PAREN)
            return false;
        return true;
    }
}
