import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;

public abstract class EiffelLexerBase extends Lexer {

    private static String[] invalidFreeOps = {
	":=.", ":=+", ":=-", "<<-", "<<+", "<<>", "<<>>"
    };

    protected EiffelLexerBase(CharStream input) {
	super(input);
    }

    public boolean IsFreeOperator() {
	boolean res = true;
	String tk = getText();
	for(int i = 0; res && i < invalidFreeOps.length; i++)
	    res = !invalidFreeOps[i].equals(tk);
	return res;
    }
}

