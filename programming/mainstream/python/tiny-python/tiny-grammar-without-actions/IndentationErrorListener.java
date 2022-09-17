import org.antlr.v4.runtime.*;

public class IndentationErrorListener extends BaseErrorListener {
    private boolean isFirstTime = true;

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer,
                            Object offendingSymbol,
                            int line, int charPositionInLine,
                            String msg,
                            RecognitionException e) {

        if (isFirstTime) {
            isFirstTime = false;
            System.out.println();
            System.err.println("ERROR:");
        }

        if (msg.startsWith(LexerWithIndentDedentInjector.TEXT_LEXER)) { // this is a custom error message from the lexer contained a pattern
            System.err.println(msg.substring(LexerWithIndentDedentInjector.TEXT_LEXER.length())); // displaying the lexer error message without the pattern
        } else { // this is a parser error message
            String startOfMessage = "line " + line + ":" + charPositionInLine + "\t ";
            if (msg.startsWith("missing INDENT")) {
                System.err.println(startOfMessage + "IndentationError: expected an indented block"); // displaying the modified parser error message
            } else if (msg.startsWith("extraneous input '<" + LexerWithIndentDedentInjector.TEXT_INSERTED_INDENT)) {
                System.err.println(startOfMessage + "IndentationError: unexpected indent"); // displaying the modified parser error message
            } else {
                System.err.println(startOfMessage + "at " + offendingSymbol + ": " + msg); // displaying the original parser error message
            }
        }
    }
}