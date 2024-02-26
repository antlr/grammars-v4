// Generated from trgen <version>

import org.antlr.v4.runtime.*;
import java.nio.charset.StandardCharsets;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.PrintStream;

public class ErrorListener extends ConsoleErrorListener
{
    public boolean had_error = false;
    private static PrintWriter stderr_utf8 = new PrintWriter(new OutputStreamWriter(System.err, StandardCharsets.UTF_8), true);
    private boolean quiet = false;
    private boolean tee = false;
    private PrintStream output = null;

    public ErrorListener(boolean q, boolean t, PrintStream o)
    {
        quiet = q;
        tee = t;
        output = o;
    }

    @Override
    public void syntaxError(Recognizer\<?, ?> recognizer,
        Object offendingSymbol,
        int line,
        int charPositionInLine,
        String msg,
        RecognitionException e)
    {
        had_error = true;
        if (tee)
        {
            output.println("line " + line + ":" + charPositionInLine + " " + msg);
        }
        if (! quiet)
        {
            stderr_utf8.println("line " + line + ":" + charPositionInLine + " " + msg);
        }
    }
}
