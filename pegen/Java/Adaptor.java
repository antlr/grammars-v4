import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;

public abstract class Adaptor extends Lexer {

    protected Adaptor(CharStream input) {
	super(input);
    }

    private int CurrentRuleType = Token.INVALID_TYPE;

    public boolean AtEnd()
    {
	return this._mode == pegen_v3_10Lexer.ACTION_MODE;
    }

    protected void handleEndAction()
    {
	int oldMode = this._mode;
	int newMode = this.popMode();
	boolean isActionWithinAction = !this._modeStack.isEmpty()
				    && newMode == pegen_v3_10Lexer.ACTION_MODE
				    && oldMode == newMode;

	if (isActionWithinAction)
	{
	    CurrentRuleType = (pegen_v3_10Lexer.ACTION);
	}
    }
}
