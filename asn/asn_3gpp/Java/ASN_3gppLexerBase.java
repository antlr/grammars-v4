import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;

public abstract class ASN_3gppLexerBase extends Lexer {

    protected ASN_3gppLexerBase(CharStream input) {
        super(input);
    }

    protected boolean IsColumnZero()
    {
        return this.getCharPositionInLine() == 0;
    }
}
