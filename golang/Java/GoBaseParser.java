import org.antlr.v4.runtime.*;

/**
 * All parser methods that used in grammar (p, prev, notLineTerminator, etc.)
 * should start with lower case char similar to parser rules.
 */
public abstract class GoBaseParser extends Parser
{

    /**
     * Returns {@code true} iff on the current index of the parser's
     * token stream a token exists on the {@code HIDDEN} channel which
     * either is a line terminator, or is a multi line comment that
     * contains a line terminator.
     *
     * @return {@code true} iff on the current index of the parser's
     * token stream a token exists on the {@code HIDDEN} channel which
     * either is a line terminator, or is a multi line comment that
     * contains a line terminator.
     */
    private boolean lineTerminatorAhead() {
        // Get the token ahead of the current index.
        int possibleIndexEosToken = this.getCurrentToken().getTokenIndex() - 1;
        Token ahead = _input.get(possibleIndexEosToken);
        if (ahead.getChannel() != Lexer.HIDDEN) {
            // We're only interested in tokens on the HIDDEN channel.
            return false;
        }

        if (ahead.getType() == TERMINATOR) {
            // There is definitely a line terminator ahead.
            return true;
        }

        if (ahead.getType() == WS) {
            // Get the token ahead of the current whitespaces.
            possibleIndexEosToken = this.getCurrentToken().getTokenIndex() - 2;
            ahead = _input.get(possibleIndexEosToken);
        }

        // Get the token's text and type.
        String text = ahead.getText();
        int type = ahead.getType();

        // Check if the token is, or contains a line terminator.
        return (type == COMMENT && (text.contains("\r") || text.contains("\n"))) ||
                (type == TERMINATOR);
    }

     /**
     * Returns {@code true} if no line terminator exists between the specified
     * token offset and the prior one on the {@code HIDDEN} channel.
     *
     * @return {@code true} if no line terminator exists between the specified
     * token offset and the prior one on the {@code HIDDEN} channel.
     */
    private boolean noTerminatorBetween(int tokenOffset) {
        BufferedTokenStream stream = (BufferedTokenStream)_input;
        List<Token> tokens = stream.getHiddenTokensToLeft(stream.LT(tokenOffset).getTokenIndex());
        
        if (tokens == null) {
            return true;
        }

        for (Token token : tokens) {
            if (token.getText().contains("\n"))
                return false;
        }

        return true;
    }

     /**
     * Returns {@code true} if no line terminator exists after any encounterd
     * parameters beyond the specified token offset and the next on the
     * {@code HIDDEN} channel.
     *
     * @return {@code true} if no line terminator exists after any encounterd
     * parameters beyond the specified token offset and the next on the
     * {@code HIDDEN} channel.
     */
    private boolean noTerminatorAfterParams(int tokenOffset) {
        BufferedTokenStream stream = (BufferedTokenStream)_input;
        int leftParams = 1;
        int rightParams = 0;
        String value;

        if (stream.LT(tokenOffset).getText().equals("(")) {
            // Scan past parameters
            while (leftParams != rightParams) {
                tokenOffset++;
                value = stream.LT(tokenOffset).getText();

                if (value.equals("(")) {
                    leftParams++;
                }
                else if (value.equals(")")) {
                    rightParams++;
                }
            }

            tokenOffset++;
            return noTerminatorBetween(tokenOffset);
        }
        
        return true;
    }

    private boolean closeBrace() {
        BufferedTokenStream stream = (BufferedTokenStream)_input;

        return stream.LT(1).getText().equals("}");
    }
}
