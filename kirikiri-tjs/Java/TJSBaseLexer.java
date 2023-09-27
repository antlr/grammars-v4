import org.antlr.v4.runtime.*;

/**
 * All lexer methods that used in grammar (IsStrictMode)
 * should start with Upper Case Char similar to Lexer rules.
 */
public abstract class TJSBaseLexer extends Lexer
{
    /**
     * Stores values of nested modes. By default mode is strict or
     * defined externally (useStrictDefault)
     */
    private Token lastToken = null;

    public TJSBaseLexer(CharStream input) {
        super(input);
    }
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
        Token next = super.nextToken();

        if (next.getChannel() == Token.DEFAULT_CHANNEL) {
            // Keep track of the last token on the default channel.
            this.lastToken = next;
        }

        return next;
    }

    /**
     * Returns {@code true} if the lexer can match a regex literal.
     */
    protected boolean IsRegexPossible() {
                                       
        if (this.lastToken == null) {
            // No token has been produced yet: at the start of the input,
            // no division is possible, so a regex literal _is_ possible.
            return true;
        }
        
        switch (this.lastToken.getType()) {
            case TJSLexer.Identifier:
            case TJSLexer.NullLiteral:
            case TJSLexer.BooleanLiteral:
            case TJSLexer.This:
            case TJSLexer.CloseBracket:
            case TJSLexer.CloseParen:
            case TJSLexer.OctalIntegerLiteral:
            case TJSLexer.DecimalLiteral:
            case TJSLexer.HexIntegerLiteral:
            case TJSLexer.StringLiteral:
            case TJSLexer.PlusPlus:
            case TJSLexer.MinusMinus:
                // After any of the tokens above, no regex literal can follow.
                return false;
            default:
                // In all other cases, a regex literal _is_ possible.
                return true;
        }
    }
}