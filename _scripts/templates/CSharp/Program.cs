// Template generated code from Antlr4BuildTasks.dotnet-antlr v <version>

using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Runtime.CompilerServices;

public class Program
{
    public static Parser Parser { get; set; }
    public static Lexer Lexer { get; set; }
    public static ITokenStream TokenStream { get; set; }
    public static IParseTree Tree { get; set; }
    public static IParseTree Parse(string input)
    {
        var str = new AntlrInputStream(input);
        var lexer = new <lexer_name>(str);
        Lexer = lexer;
        var tokens = new CommonTokenStream(lexer);
        TokenStream = tokens;
        var parser = new <parser_name>(tokens);
        Parser = parser;
        var tree = parser.<start_symbol>();
        Tree = tree;
        return tree;
    }

    static void Main(string[] args)
    {
        bool show_tree = false;
        bool show_tokens = false;
        string file_name = null;
        string input = null;
        for (int i = 0; i \< args.Length; ++i)
        {
            if (args[i].Equals("-tokens"))
            {
                show_tokens = true;
                continue;
            }
            else if (args[i].Equals("-tree"))
            {
                show_tree = true;
                continue;
            }
            else if (args[i].Equals("-input"))
                input = args[++i];
            else if (args[i].Equals("-file"))
                file_name = args[++i];
        }
        ICharStream str = null;
        if (input == null && file_name == null)
        {
            str = CharStreams.fromStream(System.Console.OpenStandardInput());
        } else if (input != null)
        {
            str = CharStreams.fromString(input);
        } else if (file_name != null)
        {
            str = CharStreams.fromPath(file_name);
        }
        var lexer = new <lexer_name>(str);
        if (show_tokens)
        {
            StringBuilder new_s = new StringBuilder();
            for (int i = 0; ; ++i)
            {
                var ro_token = lexer.NextToken();
                var token = (CommonToken)ro_token;
                token.TokenIndex = i;
                new_s.AppendLine(token.ToString());
                if (token.Type == Antlr4.Runtime.TokenConstants.EOF)
                    break;
            }
            System.Console.Error.WriteLine(new_s.ToString());
            lexer.Reset();
        }
        var tokens = new CommonTokenStream(lexer);
        var parser = new <parser_name>(tokens);
        var listener_lexer = new ErrorListener\<int>();
        var listener_parser = new ErrorListener\<IToken>();
        lexer.AddErrorListener(listener_lexer);
        parser.AddErrorListener(listener_parser);
        DateTime before = DateTime.Now;
        var tree = parser.<start_symbol>();
        DateTime after = DateTime.Now;
        System.Console.Error.WriteLine("Time: " + (after - before));
        if (listener_lexer.had_error || listener_parser.had_error)
        {
            System.Console.Error.WriteLine("Parse failed.");
        }
        else
        {
            System.Console.Error.WriteLine("Parse succeeded.");
        }
        if (show_tree)
        {
            System.Console.Error.WriteLine(tree.ToStringTree(parser));
        }
        System.Environment.Exit(listener_lexer.had_error || listener_parser.had_error ? 1 : 0);
    }
}
