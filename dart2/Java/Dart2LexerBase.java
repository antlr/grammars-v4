import java.util.List;
import org.antlr.v4.runtime.*;

public abstract class Dart2LexerBase extends Lexer
{
    protected Dart2LexerBase(CharStream input) {
	super(input);
    }

    protected boolean CheckNotOpenBrace()
    {
	return _input.LA(1) != '{';
    }
}
