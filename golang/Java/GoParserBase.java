import java.util.List;
import org.antlr.v4.runtime.*;

/**
 * All parser methods that used in grammar (p, prev, notLineTerminator, etc.)
 * should start with lower case char similar to parser rules.
 */
public abstract class GoParserBase extends Parser
{
    protected GoParserBase(TokenStream input) {
        super(input);
    }


    /**
     * Returns true if the current Token is a closing bracket (")" or "}")
     */
    protected boolean closingBracket()
    {
        BufferedTokenStream stream = (BufferedTokenStream)_input;
        int la = stream.LA(1);
        return la == GoLexer.R_PAREN || la == GoLexer.R_CURLY || la == Token.EOF;
    }

    protected boolean isType()
    {
        BufferedTokenStream stream = (BufferedTokenStream)_input;
        int nextTokenType = stream.LA(1);
        return nextTokenType != GoLexer.IDENTIFIER;
    }
}
