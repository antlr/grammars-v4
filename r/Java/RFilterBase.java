import org.antlr.v4.runtime.*;

public abstract class RFilterBase extends Parser {
    /*
        Base class for RFilter parser with state management for curly braces
        and helper methods for token manipulation.
    */
    protected int curlies = 0;

    public RFilterBase(TokenStream input) {
        super(input);
    }

    protected void hideToken(Token token) {
        /*
            Set a token to the hidden channel.
            This is the Python equivalent of Java's token.setChannel(Token.HIDDEN_CHANNEL)
        */
        if (token instanceof CommonToken) {
            ((CommonToken) token).setChannel(Token.HIDDEN_CHANNEL);
        }
    }
}