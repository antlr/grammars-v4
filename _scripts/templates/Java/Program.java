// Template generated code from Antlr4BuildTasks.dotnet-antlr v <version>

import java.io.FileNotFoundException;
import java.io.IOException;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;

public class Program {
    public static void main(String[] args) throws  FileNotFoundException, IOException
    {
        boolean show_tree = false;
        boolean show_tokens = false;
        String file_name = null;
        String input = null;
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
            str = CharStreams.fromFileName(file_name);
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
        parser.removeParseListeners();
        parser.addErrorListener(listener);
        lexer.addErrorListener(lexer_listener);
        ParseTree tree = parser.<start_symbol>();
        if (listener.had_error || lexer_listener.had_error)
            System.out.println("Parse failed.");
        else
            System.out.println("Parse succeeded.");
        if (show_tree)
        {
            System.out.println(tree.toStringTree(parser));
        }
        java.lang.System.exit(listener.had_error || lexer_listener.had_error ? 1 : 0);
    }
}
