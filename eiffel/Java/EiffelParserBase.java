import org.antlr.v4.runtime.*;

public abstract class EiffelParserBase extends Parser {

    public EiffelParserBase(TokenStream input){
        super(input);
    }

    public boolean IsNone()
    {
        return "none".equalsIgnoreCase(_input.LT(2).getText());
    }

    public boolean IsTuple()
    {
        return "tuple".equalsIgnoreCase(_input.LT(1).getText());
    }
}


    