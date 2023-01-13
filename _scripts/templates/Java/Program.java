// Template generated code from trgen <version>

import java.io.FileNotFoundException;
import java.io.IOException;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import java.time.Instant;
import java.time.Duration;
import java.nio.charset.StandardCharsets;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

public class Program {
    private static PrintWriter stdout_utf8 = new PrintWriter(new OutputStreamWriter(System.out, StandardCharsets.UTF_8), true);
    private static PrintWriter stderr_utf8 = new PrintWriter(new OutputStreamWriter(System.err, StandardCharsets.UTF_8), true);
    public static void main(String[] args) throws  FileNotFoundException, IOException
    {
        boolean show_tree = false;
        boolean show_tokens = false;
        String file_name = null;
        String input = null;
        java.nio.charset.Charset charset = null;
        for (int i = 0; i \< args.length; ++i)
        {
            if (args[i].equals("-tokens"))
            {
                show_tokens = true;
                continue;
            }
            else if (args[i].equals("-tree"))
            {
                show_tree = true;
                continue;
            }
            else if (args[i].equals("-input"))
                input = args[++i];
            else if (args[i].equals("-file"))
                file_name = args[++i];
            else if (args[i].equals("-encoding"))
            {
                charset = java.nio.charset.Charset.forName(args[++i]);
            }
        }
        CharStream str = null;
        if (input == null && file_name == null)
        {
            str = CharStreams.fromStream(System.in);
        } else if (input != null)
        {
            str = CharStreams.fromString(input);
        } else if (file_name != null)
        {
            if (charset == null)
            str = CharStreams.fromFileName(file_name);
            else
                str = CharStreams.fromFileName(file_name, charset);
        }
<if (case_insensitive_type)>
        str = new CaseChangingCharStream(str, "<case_insensitive_type>" == "Upper");
<endif>
        <lexer_name> lexer = new <lexer_name>(str);
        if (show_tokens)
        {
            StringBuilder new_s = new StringBuilder();
            for (int i = 0; ; ++i)
            {
                var ro_token = lexer.nextToken();
                var token = (CommonToken)ro_token;
                token.setTokenIndex(i);
                new_s.append(token.toString());
                new_s.append(System.getProperty("line.separator"));
                if (token.getType() == IntStream.EOF)
                    break;
            }
            System.out.println(new_s.toString());
            lexer.reset();
        }
        var tokens = new CommonTokenStream(lexer);
        <parser_name> parser = new <parser_name>(tokens);
        ErrorListener lexer_listener = new ErrorListener();
        ErrorListener listener = new ErrorListener();
        parser.removeErrorListeners();
        lexer.removeErrorListeners();
        parser.addErrorListener(listener);
        lexer.addErrorListener(lexer_listener);
        Instant start = Instant.now();
        ParseTree tree = parser.<start_symbol>();
        Instant finish = Instant.now();
        long timeElapsed = Duration.between(start, finish).toMillis();
        stderr_utf8.println("Time: " + (timeElapsed * 1.0) / 1000.0);
        if (listener.had_error || lexer_listener.had_error) {
            // Listener will have already printed the error(s) to stdout.
            stderr_utf8.println("Parse failed.");
        } else {
            stderr_utf8.println("Parse succeeded.");
            if (show_tree)
            {
                stdout_utf8.println(tree.toStringTree(parser));
            }
        }
        java.lang.System.exit(listener.had_error || lexer_listener.had_error ? 1 : 0);
    }
}
