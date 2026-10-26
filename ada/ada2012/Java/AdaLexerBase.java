import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;

public abstract class AdaLexerBase extends Lexer {
    private int _lastTokenType = 0;

    protected AdaLexerBase(CharStream input) {
        super(input);
    }

    @Override
    public Token nextToken() {
        Token token = super.nextToken();
        if (token.getChannel() == Lexer.DEFAULT_TOKEN_CHANNEL) {
            _lastTokenType = token.getType();
        }
        return token;
    }

    protected boolean IsCharLiteralAllowed() {
        // In Ada, a tick after an identifier, closing paren, or 'all' keyword
        // is an attribute tick, not the start of a character literal.
        return _lastTokenType != AdaLexer.IDENTIFIER_
            && _lastTokenType != AdaLexer.RP
            && _lastTokenType != AdaLexer.ALL;
    }
}
