import java.util.List;
import org.antlr.v4.runtime.*;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.Set;

public abstract class LuaParserBase extends Parser
{
    protected LuaParserBase(TokenStream input) {
        super(input);
    }

    protected boolean IsFunctionCall()
    {
        BufferedTokenStream stream = (BufferedTokenStream)_input;
        var la = stream.LT(1);
	if (la.getType() != LuaLexer.NAME) return false;
	la = stream.LT(2);
	if (la.getType() == LuaLexer.OP) return false;
	return true;
    }
}
