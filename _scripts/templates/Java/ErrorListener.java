// Template generated code from trgen <version>

import org.antlr.v4.runtime.*;
import java.nio.charset.StandardCharsets;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

public class ErrorListener extends ConsoleErrorListener
{
    public boolean had_error = false;
    private static PrintWriter stdout_utf8 = new PrintWriter(new OutputStreamWriter(System.out, StandardCharsets.UTF_8), true);
    
    @Override
    public void syntaxError(Recognizer\<?, ?> recognizer,
        Object offendingSymbol,
        int line,
        int charPositionInLine,
        String msg,
        RecognitionException e)
    {
        had_error = true;
        stdout_utf8.println("line " + line + ":" + charPositionInLine + " " + msg);
    }
}
