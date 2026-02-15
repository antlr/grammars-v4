import org.antlr.v4.runtime.*;
import java.io.PrintStream;

public class ErrorListener extends BaseErrorListener {
    public boolean had_error;
    private boolean _quiet;
    private boolean _tee;
    private PrintStream _out;

    public ErrorListener(boolean quiet, boolean tee, PrintStream out) {
        _quiet = quiet;
        _tee = tee;
        _out = out;
    }

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line,
                            int col, String msg, RecognitionException e) {
        String fileName = "<unknown>";
        int lineAdjusted = line;

        // Get token stream.
        if (recognizer instanceof Parser) {
            Parser p = (Parser) recognizer;
            TokenStream ts = p.getInputStream();
            Token q = (Token) offendingSymbol;
            TokenSource sou = q.getTokenSource();
            CommonTokenStream ts2 = (CommonTokenStream) p.getInputStream();
            // Search back from offending symbol index to find last LineDirective.
            int ind = q.getTokenIndex();
            for (int j = ind; j >= 0; j--) {
                Token t = ts2.get(j);
                if (t == null) break;
                if (t.getType() == CLexer.LineDirective) {
                    // Found it.
                    String txt = t.getText();
                    String[] parts = txt.split("\\s+");
                    if (parts.length >= 3) {
                        // Get line number from directive.
                        try {
                            int dirLine = Integer.parseInt(parts[1]);
                            // Get line number of directive.
                            int lineDirective = t.getLine();
                            // Get line difference from line directive.
                            int lineDiff = line - lineDirective;
                            // Adjust line number.
                            lineAdjusted = lineDiff + dirLine - 1;
                            fileName = parts[2].trim();
                        } catch (NumberFormatException ex) {
                            // Ignore parse errors
                        }
                    }
                    break;
                }
            }
        }
        had_error = true;
        if (_tee && _out != null) {
            _out.println(fileName + " line " + lineAdjusted + ", .p " + line + ":" + col + " " + msg);
        }
        if (!_quiet) {
            System.err.println(fileName + " line " + lineAdjusted + ", .p " + line + ":" + col + " " + msg);
        }
    }
}
