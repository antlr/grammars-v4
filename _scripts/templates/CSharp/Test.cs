// Template generated code from trgen <version>

using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Tree;
using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Runtime.CompilerServices;
using System.Collections.Generic;
using System.Net.Sockets;

public class Program
{
    public static <parser_name> Parser { get; set; }
    public static ErrorListener\<IToken> ParserErrorListener { get; set; }
    public static Lexer Lexer { get; set; }
    public static ErrorListener\<int> LexerErrorListener { get; set; }
    public static ITokenStream TokenStream { get; set; }
    public static ICharStream CharStream { get; set; }
    public static IParseTree Tree { get; set; }
    public static string StartSymbol { get; set; } = "<start_symbol>";
    public static string Input { get; set; }
    public static void SetupParse2(string input, bool quiet = false)
    {
        ICharStream str = new AntlrInputStream(input);
        CharStream = str;
        var lexer = new <lexer_name>(str);
        Lexer = lexer;
        var tokens = new CommonTokenStream(lexer);
        TokenStream = tokens;
        var parser = new <parser_name>(tokens);
        Parser = parser;
        var listener_lexer = new ErrorListener\<int>(false, System.Console.Out);
        var listener_parser = new ErrorListener\<IToken>(false, System.Console.Out);
        LexerErrorListener = listener_lexer;
        ParserErrorListener = listener_parser;
        lexer.RemoveErrorListeners();
        parser.RemoveErrorListeners();
        lexer.AddErrorListener(listener_lexer);
        parser.AddErrorListener(listener_parser);
    }

    public static IParseTree Parse2()
    {
        var tree = Parser.<start_symbol>();
        Input = Lexer.InputStream.ToString();
        TokenStream = Parser.TokenStream;
        Tree = tree;
        return tree;
    }

    public static bool AnyErrors()
    {
        return ParserErrorListener.had_error || LexerErrorListener.had_error;
    }

    public static IParseTree Parse(string input)
    {
        ICharStream str = new AntlrInputStream(input);
        CharStream = str;
        var lexer = new <lexer_name>(str);
        Lexer = lexer;
        var tokens = new CommonTokenStream(lexer);
        TokenStream = tokens;
        var parser = new <parser_name>(tokens);
        Parser = parser;
        var listener_lexer = new ErrorListener\<int>(false, System.Console.Out);
        var listener_parser = new ErrorListener\<IToken>(false, System.Console.Out);
        lexer.RemoveErrorListeners();
        parser.RemoveErrorListeners();
        lexer.AddErrorListener(listener_lexer);
        parser.AddErrorListener(listener_parser);
        var tree = parser.<start_symbol>();
        Input = lexer.InputStream.ToString();
        TokenStream = parser.TokenStream;
        Tree = tree;
        return tree;
    }

    static bool shunt_output = false;
    static bool show_profile = false;
    static bool show_tree = false;
    static bool show_tokens = false;
    static bool show_trace = false;
    static bool old = false;
    static bool two_byte = false;
    static int exit_code = 0;
    static Encoding encoding = null;
    static int string_instance = 0;
    static string prefix = "";
    static bool quiet = false;

    static void Main(string[] args)
    {
        List\<bool> is_fns = new List\<bool>();
        List\<string> inputs = new List\<string>();
        for (int i = 0; i \< args.Length; ++i)
        {
            if (args[i].Equals("-profile"))
            {
                show_profile = true;
                continue;
            }
            else if (args[i].Equals("-tokens"))
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
            else if (args[i].Equals("-prefix"))
            {
                prefix = args[++i] + " ";
            }
            else if (args[i].Equals("-input"))
            {
                inputs.Add(args[++i]);
                is_fns.Add(false);
            }
            else if (args[i].Equals("-shunt"))
            {
                shunt_output = true;
                continue;
            }
            else if (args[i].Equals("-encoding"))
            {
                ++i;
                encoding = Encoding.GetEncoding(
                    args[i],
                    new EncoderReplacementFallback("(unknown)"),
                    new DecoderReplacementFallback("(error)"));
                if (encoding == null)
                    throw new Exception(@"Unknown encoding. Must be an Internet Assigned Numbers Authority (IANA) code page name. https://www.iana.org/assignments/character-sets/character-sets.xhtml");
            }
            else if (args[i] == "-x")
            {
                for (; ; )
                {
                    var line = System.Console.In.ReadLine();
                    line = line?.Trim();
                    if (line == null || line == "")
                    {
                        break;
                    }
                    inputs.Add(line);
                    is_fns.Add(true);
                }
            }
            else if (args[i] == "-q")
            {
                quiet = true;
            }
            else if (args[i] == "-trace")
            {
                show_trace = true;
            }
            else
            {
                 inputs.Add(args[i]);
                 is_fns.Add(true);
            }
        }
        if (inputs.Count() == 0)
        {
            ParseStdin();
        }
        else
        {
            DateTime before = DateTime.Now;
            for (int f = 0; f \< inputs.Count(); ++f)
            {
                if (is_fns[f])
                    ParseFilename(inputs[f], f);
                else
                    ParseString(inputs[f], f);
            }
            DateTime after = DateTime.Now;
            System.Console.Error.WriteLine("Total Time: " + (after - before).TotalSeconds);
        }
        Environment.ExitCode = exit_code;
    }

    static void ParseStdin()
    {
        ICharStream str = null;
        str = CharStreams.fromStream(System.Console.OpenStandardInput());
        DoParse(str, "stdin", 0);
    }

    static void ParseString(string input, int row_number)
    {
        ICharStream str = null;
        str = CharStreams.fromString(input);
        DoParse(str, "string" + string_instance++, row_number);
    }

    static void ParseFilename(string input, int row_number)
    {
        ICharStream str = null;
        if (two_byte)
            str = new TwoByteCharStream(input);
        else if (old)
        {
            FileStream fs = new FileStream(input, FileMode.Open);
            str = new Antlr4.Runtime.AntlrInputStream(fs);
        }
        else if (encoding == null)
            str = CharStreams.fromPath(input);
        else
            str = CharStreams.fromPath(input, encoding);
        DoParse(str, input, row_number);
    }

    static void DoParse(ICharStream str, string input_name, int row_number)
    {
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
        var output = shunt_output ? new StreamWriter(input_name + ".errors") : System.Console.Out;
        var listener_lexer = new ErrorListener\<int>(quiet, output);
        var listener_parser = new ErrorListener\<IToken>(quiet, output);
        lexer.RemoveErrorListeners();
        parser.RemoveErrorListeners();
        lexer.AddErrorListener(listener_lexer);
        parser.AddErrorListener(listener_parser);
        DateTime before = DateTime.Now;
        if (show_profile)
        {
            parser.Profile = true;
        }
        if (show_trace)
        {
            parser.Trace = true;
            ParserATNSimulator.trace_atn_sim = true;
        }
        var tree = parser.<start_symbol>();
        DateTime after = DateTime.Now;
        var result = "";
        if (listener_lexer.had_error || listener_parser.had_error)
        {
            result = "fail";
            exit_code = 1;
        }
        else
        {
            result = "success";
        }
        if (show_tree)
        {
            if (shunt_output)
            {
                System.IO.File.WriteAllText(input_name + ".tree", tree.ToStringTree(parser));
            } else
            {
            System.Console.Out.WriteLine(tree.ToStringTree(parser));
        }
        }
        if (show_profile)
        {
            System.Console.Out.WriteLine(String.Join(",\n\r", parser.ParseInfo.getDecisionInfo().Select(d => d.ToString())));
        }
        if (!quiet)
        {
            System.Console.Error.WriteLine(prefix + "CSharp " + row_number + " " + input_name + " " + result + " " + (after - before).TotalSeconds);
        }
        if (shunt_output) output.Close();
    }
}
