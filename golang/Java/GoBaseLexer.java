import org.antlr.v4.runtime.*;

/**
 * All lexer methods that used in grammar (IsStrictMode)
 * should start with Upper Case Char similar to Lexer rules.
 */
public abstract class GoBaseLexer extends Lexer
{
    // The most recently produced token.
    private Token lastToken = null;

    /**
     * Return the next token from the character stream and records this last
     * token in case it resides on the default channel. This recorded token
     * is used to determine when the lexer could possibly match a regex
     * literal.
     *
     * @return the next token from the character stream.
     */
    @Override
    public Token nextToken() {

        // Get the next token.
        Token next = super.nextToken();

        if (next.getChannel() == Token.DEFAULT_CHANNEL) {
            // Keep track of the last token on the default channel.
            this.lastToken = next;
        }

        return next;
    }
}
