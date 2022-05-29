/*
 * Project      : an ErrorListener class to collect and display indentation errors
 */

import org.antlr.v4.runtime.*;

public class IndentationErrorListener extends BaseErrorListener {
    private static StringBuilder _sbError;

    public static void lexerError(String msg) {
        addError(msg);
    }

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer,
                            Object offendingSymbol,
                            int line, int charPositionInLine,
                            String msg,
                            RecognitionException e) {

        final String startOfMessage = " line " + line + ":\t ";
        final String sINDENT = PythonLexer.VOCABULARY.getDisplayName(PythonLexer.INDENT);

        // ************************************************************
        // *** Not exact matches! This is only for a demonstration. ***
        // ************************************************************
        if (msg.startsWith("missing INDENT at ")) {
            addError(startOfMessage + "expected an indented block");
        } else if (msg.startsWith("mismatched input '<" + sINDENT + ">") ||
                msg.startsWith("extraneous input '<" + sINDENT + ">")) {

            addError(startOfMessage + "unexpected indent");
        }
    }

    private static void addError(String errorMsg) {
        if (_sbError == null) {
            _sbError = new StringBuilder();
            _sbError.append(System.lineSeparator());
            _sbError.append("INDENTATION ERROR:").append(System.lineSeparator());
        }
        _sbError.append(errorMsg).append(System.lineSeparator());
    }

    public static void displayErrors() {
        if (_sbError != null) {
            System.err.println(_sbError);
            _sbError = null;
        }
    }
}
