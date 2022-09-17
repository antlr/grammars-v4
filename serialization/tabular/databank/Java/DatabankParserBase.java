import java.util.List;
import org.antlr.v4.runtime.*;

/**
 * All parser methods that used in grammar (p, prev, notLineTerminator, etc.)
 * should start with lower case char similar to parser rules.
 */
public abstract class DatabankParserBase extends Parser
{
    protected DatabankParserBase(TokenStream input) {
        super(input);
    }


    /**
     * Returns true if the current Token is a closing bracket (")" or "}")
     */
    protected boolean isdatatype()
    {
	String text = _input.LT(-1).getText();
	return text.equals("-1") || text.equals("-4") || text.equals("-12");
    }
}
