// Template generated code from trgen <version>

using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Runtime.CompilerServices;

<if(has_name_space)>namespace <name_space>
{<endif>

public class Program
{
    public static Parser Parser { get; set; }
    public static Lexer Lexer { get; set; }
    public static ITokenStream TokenStream { get; set; }
    public static IParseTree Tree { get; set; }
    public static string StartSymbol { get; set; } = "<start_symbol>";
    public static string Input { get; set; }

    static void Main(string[] args)
    {
        bool show_tree = false;
        bool show_tokens = false;
        bool old = false;
        bool two_byte = false;
        string file_name = null;
        string input = null;
        for (int i = 0; i \< args.Length; ++i)
        {
            if (args[i].Equals("-tokens"))
            {
                show_tokens = true;
                continue;
            }
            else if (args[i].Equals("-two-byte"))
            {
                two_byte = true;
                continue;
            }
            else if (args[i].Equals("-old"))
            {
                old = true;
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
            StringBuilder sb = new StringBuilder();
            int ch;
            while ((ch = System.Console.Read()) != -1)
            {
                sb.Append((char)ch);
            }
            input = sb.ToString();
            str = new Antlr4.Runtime.AntlrInputStream(
                new MemoryStream(Encoding.UTF8.GetBytes(input ?? "")));
        }
        else if (input != null)
        {
            str = new Antlr4.Runtime.AntlrInputStream(
                    new MemoryStream(Encoding.UTF8.GetBytes(input ?? "")));
        } else if (file_name != null)
        {
            FileStream fs = new FileStream(file_name, FileMode.Open);
            str = new Antlr4.Runtime.AntlrInputStream(fs);
        }
<if (case_insensitive_type)>
        str = new Antlr4.Runtime.CaseChangingCharStream(str, "<case_insensitive_type>" == "Upper");
< endif >
        var lexer = new Test.<lexer_name>(str);
        if (show_tokens)
        {
            StringBuilder new_s = new StringBuilder();
            for (int i = 0; ; ++i)
            {
                var ro_token = lexer.NextToken();
                var token = (CommonToken)ro_token;
                token.TokenIndex = i;
                new_s.AppendLine(token.ToString());
                if (token.Type == Antlr4.Runtime.TokenConstants.Eof)
                    break;
            }
            System.Console.Error.WriteLine(new_s.ToString());
            lexer.Reset();
        }
        var tokens = new CommonTokenStream(lexer);
        var parser = new Test.<parser_name>(tokens);
        DateTime before = DateTime.Now;
        var tree = parser.<start_symbol>();
        DateTime after = DateTime.Now;
        System.Console.Error.WriteLine("Time: " + (after - before));
        if (parser.NumberOfSyntaxErrors > 0)
        {
            System.Console.Error.WriteLine("Parse failed.");
        }
        else
        {
            System.Console.Error.WriteLine("Parse succeeded.");
            if (show_tree)
            {
                System.Console.Out.WriteLine(tree.ToStringTree(parser));
            }
        }
        System.Environment.Exit(parser.NumberOfSyntaxErrors > 0 ? 1 : 0);
    }
}

<if(has_name_space)>}<endif>
