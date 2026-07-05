// Generated from trgen <version>

using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Tree;
using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Runtime.CompilerServices;
using System.Collections.Generic;
using System.Net;
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

    static bool tee = false;
    static bool show_profile = false;
    static bool show_tokens = false;
    static bool show_trace = false;
    static bool show_tree = false;
    static bool old = false;
    static bool two_byte = false;
    static int exit_code = 0;
    static string file_encoding = "<file_encoding>";
    static bool binary = <binary>;
    static int string_instance = 0;
    static string prefix = "";
    static bool quiet = false;
    static long total_tokens = 0;
    static double total_parse_seconds = 0;
    static long first_file_tokens = 0;
    static double first_file_parse_seconds = 0;

    static void Main(string[] args)
    {
        List\<bool> is_fns = new List\<bool>();
        List\<string> inputs = new List\<string>();
        for (int i = 0; i \< args.Length; ++i)
        {
            if (args[i].Equals("-profile"))
            {
                show_profile = true;
            }
            else if (args[i].Equals("-tokens"))
            {
                show_tokens = true;
            }
            else if (args[i].Equals("-two-byte"))
            {
                two_byte = true;
            }
            else if (args[i].Equals("-old"))
            {
                old = true;
            }
            else if (args[i].Equals("-tree"))
            {
                show_tree = true;
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
            else if (args[i].Equals("-tee"))
            {
                tee = true;
            }
            else if (args[i].Equals("-encoding"))
            {
                ++i;
                file_encoding = args[i];
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
            if (!quiet)
            {
                var overall_seconds = (after - before).TotalSeconds;
                var warm_tokens = total_tokens - first_file_tokens;
                var warm_seconds = total_parse_seconds - first_file_parse_seconds;
                var warm_tps = (inputs.Count() > 1 && warm_seconds > 0)
                    ? ((long)(warm_tokens / warm_seconds)).ToString()
                    : "n.a.";
                var first_tps = first_file_parse_seconds > 0 ? (first_file_tokens / first_file_parse_seconds) : 0;
                var speedup = (inputs.Count() > 1 && warm_seconds > 0 && first_tps > 0)
                    ? ((warm_tokens / warm_seconds) / first_tps).ToString("F2")
                    : "n.a.";
                System.Console.Error.WriteLine(prefix + "PT: " + total_parse_seconds);
                System.Console.Error.WriteLine(prefix + "OT: " + (overall_seconds - total_parse_seconds));
                System.Console.Error.WriteLine(prefix + "TT: " + overall_seconds);
                System.Console.Error.WriteLine(prefix + "TPS: " + (long)(total_tokens / total_parse_seconds));
                System.Console.Error.WriteLine(prefix + "Post-warmup TPS: " + warm_tps);
                System.Console.Error.WriteLine(prefix + "Post-warmup speed up: " + speedup);
            }
        }
        Environment.ExitCode = exit_code;
    }

    static void ParseStdin()
    {
        StringBuilder sb = new StringBuilder();
        int ch;
        while ((ch = System.Console.Read()) != -1)
        {
            sb.Append((char)ch);
        }
        var input = sb.ToString();
        var str = new Antlr4.Runtime.AntlrInputStream(
            new MemoryStream(Encoding.UTF8.GetBytes(input ?? "")));
        DoParse(str, "stdin", 0);
    }

    static void ParseString(string input, int row_number)
    {
        ICharStream str = null;
        str = new Antlr4.Runtime.AntlrInputStream(
            new MemoryStream(Encoding.UTF8.GetBytes(input ?? "")));
        DoParse(str, "string" + string_instance++, row_number);
    }

    static void ParseFilename(string input, int row_number)
    {
        ICharStream str = null;
        FileStream fs = new FileStream(input, FileMode.Open);
        str = new Antlr4.Runtime.AntlrInputStream(fs);
        DoParse(str, input, row_number);
    }

    static void DoParse(ICharStream str, string input_name, int row_number)
    {
        if (binary) str = new BinaryCharStream(str);
<if (case_insensitive_type)>
        str = new Antlr4.Runtime.CaseChangingCharStream(str, "<case_insensitive_type>" == "Upper");
<endif>
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
                if (token.Type == Antlr4.Runtime.TokenConstants.Eof)
                    break;
            }
            System.Console.Error.WriteLine(new_s.ToString());
            lexer.Reset();
        }
        var tokens = new CommonTokenStream(lexer);
        var parser = new <parser_name>(tokens);
        var output = tee ? new StreamWriter(input_name + ".errors") : System.Console.Error;
        var listener_lexer = new ErrorListener\<int>(quiet, tee, output);
        var listener_parser = new ErrorListener\<IToken>(quiet, tee, output);
        lexer.RemoveErrorListeners();
        parser.RemoveErrorListeners();
        lexer.AddErrorListener(listener_lexer);
        parser.AddErrorListener(listener_parser);
        if (show_trace)
        {
            // parser.Trace = true;
            // ATN tracing missing.
        }
        DateTime before = DateTime.Now;
        var tree = parser.<start_symbol>();
        DateTime after = DateTime.Now;
        var parse_seconds = (after - before).TotalSeconds;
        total_parse_seconds += parse_seconds;
        var token_count = tokens.Size;
        total_tokens += token_count;
        if (row_number == 0)
        {
            first_file_tokens = token_count;
            first_file_parse_seconds = parse_seconds;
        }
        var result = "";
        if (parser.NumberOfSyntaxErrors > 0)
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
            if (tee)
            {
                System.IO.File.WriteAllText(input_name + ".tree", tree.ToStringTree(parser));
            } else
            {
                System.Console.Error.WriteLine(tree.ToStringTree(parser));
            }
        }
        if (!quiet)
        {
            System.Console.Error.WriteLine(prefix + "Antlr4cs " + row_number + " " + input_name + " " + result + " " + parse_seconds + " s " + token_count + " tokens " + (long)(token_count / parse_seconds) + " tps");
        }
        if (tee) output.Close();
    }
}

<if(has_name_space)>}<endif>
