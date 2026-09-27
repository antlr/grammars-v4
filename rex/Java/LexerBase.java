import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;

public abstract class LexerBase extends Lexer {
    protected LexerBase(CharStream input) { super(input); }

    public boolean Check1() { return _input.LA(1) != ':'; }
}
