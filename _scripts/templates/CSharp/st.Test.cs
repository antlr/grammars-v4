// Generated from trgen <version>

using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using UtfUnknown;
<if(has_name_space)>namespace <name_space>
{<endif>

public class Program
{
    public static <parser_name> Parser { get; set; }
    public static ErrorListener\<IToken> ParserErrorListener { get; set; }
    public static <lexer_name> Lexer { get; set; }
    public static ErrorListener\<int> LexerErrorListener { get; set; }
    public static ITokenStream TokenStream { get; set; }
    public static ICharStream CharStream { get; set; }
    public static IParseTree Tree { get; set; }
    public static List\<IParseTree> Trees { get; set; }
    public static string StartSymbol { get; set; } = "<start_symbol>";
    public static string Input { get; set; }
    public static bool HeatMap { get; set; } = false;
    public static void SetupParse2(string input, string fn, bool quiet = false)
    {
        var code_point_stream = new CodePointCharStream(input);
        code_point_stream.name = fn;
        ICharStream str = code_point_stream;
        CharStream = str;
        var lexer = new <lexer_name>(str);
        Lexer = lexer;
        CommonTokenStream tokens = null;
        if (HeatMap) {
            tokens = new ProfilingCommonTokenStream(lexer);
        }
        else {
            tokens = new CommonTokenStream(lexer);
        }
        TokenStream = tokens;
        ((CodePointCharStream)(lexer.InputStream)).name = fn;
        var parser = new MyParser(tokens);
        Parser = parser;
        var listener_lexer = new ErrorListener\<int>(false, false, System.Console.Error);
        var listener_parser = new ErrorListener\<IToken>(false, false, System.Console.Error);
        LexerErrorListener = listener_lexer;
        ParserErrorListener = listener_parser;
        lexer.RemoveErrorListeners();
        parser.RemoveErrorListeners();
        lexer.AddErrorListener(listener_lexer);
        parser.AddErrorListener(listener_parser);
    }

    public static List\<Tuple\<string, IParseTree>> Parse2()
    {
        var args = Environment.GetCommandLineArgs().ToList();
        int s_ambig_index = args.FindIndex(a => a.StartsWith("-ambig"));
        int d_ambig_index = args.FindIndex(a => a.StartsWith("--ambig"));
        bool ambig = s_ambig_index >= 0 || d_ambig_index >= 0;
        if (s_ambig_index >= 0 && (args[s_ambig_index].StartsWith("-ambig=")))
        {
            ambig_decisions = new HashSet\<int>();
            int prefix_len = 7;
            foreach (var part in args[s_ambig_index].Substring(prefix_len).Split(','))
                if (int.TryParse(part.Trim(), out int d))
                    ambig_decisions.Add(d);
        }
        else if (d_ambig_index >= 0 && (args[d_ambig_index].StartsWith("-ambig=")))
        {
            ambig_decisions = new HashSet\<int>();
            int prefix_len = 8;
            foreach (var part in args[d_ambig_index].Substring(prefix_len).Split(','))
                if (int.TryParse(part.Trim(), out int d))
                    ambig_decisions.Add(d);
        }
        int limit_index = args?.FindIndex(a => a == "--limit" || a.StartsWith("--limit=")) ?? -1;
        if (limit_index >= 0)
        {
            if (args[limit_index].StartsWith("--limit="))
                int.TryParse(args[limit_index].Substring(8), out limit);
            else if (limit_index + 1 \< args.Count)
                int.TryParse(args[limit_index + 1], out limit);
        }
        var result = new List\<Tuple\<string, IParseTree>>();

        if (ambig) {
            Parser.Profile = true;
            var tree = Parser.<start_symbol>();
            var decisions = Parser.ParseInfo.getDecisionInfo().Where(d => d.ambiguities.Any()).ToList();
            foreach (var decision in decisions)
            {
                var am = decision.ambiguities;
                foreach (AmbiguityInfo ai in am)
                {
                    var parser_decision = ai.decision;
                    if (ambig_decisions != null && !ambig_decisions.Contains(parser_decision))
                        continue;
                    var parser_alts = ai.ambigAlts;
                    var parser_startIndex = ai.startIndex;
                    var parser_stopIndex = ai.stopIndex;
                    var p = Parser.RuleNames.Select((value, index) => new { value, index })
                        .Where(pair => (pair.value == "prog"))
                        .Select(pair => pair.index).First();
                    var parser_startRuleIndex = p;
                    var parse_trees = ((MyParser)Parser).getAllPossibleParseTrees(
                        parser_decision,
                        parser_alts,
                        parser_startIndex,
                        parser_stopIndex,
                        parser_startRuleIndex,
                        limit > 0 ? Math.Max(0, limit - result.Count) : 0);
                    foreach (var t in parse_trees)
                    {
                        var n = (ambig_decisions == null || !ambig_decisions.Any()) ? t.Item1 : null;
                        result.Add(new Tuple\<string, IParseTree>(n, t.Item2));
                        if (limit > 0 && result.Count >= limit) break;
                    }
                    if (limit > 0 && result.Count >= limit) break;
                }
                if (limit > 0 && result.Count >= limit) break;
            }
            Input = Lexer.InputStream.ToString();
            TokenStream = Parser.TokenStream;
        } else {
            var tree = Parser.<start_symbol>();
            Input = Lexer.InputStream.ToString();
            TokenStream = Parser.TokenStream;
            result.Add(new Tuple\<string, IParseTree>(null, tree));
        }
        return result;
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
        CommonTokenStream tokens = null;
        if (show_hit) {
            tokens = new ProfilingCommonTokenStream(lexer);
        }
        else {
            tokens = new CommonTokenStream(lexer);
        }
        TokenStream = tokens;
        var parser = new <parser_name>(tokens);
        Parser = parser;
        var listener_lexer = new ErrorListener\<int>(false, false, System.Console.Error);
        var listener_parser = new ErrorListener\<IToken>(false, false, System.Console.Error);
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

    static bool tee = false;
    static bool show_diagnostic = false;
    static bool show_hit = false;
    static bool show_ambig = false;
    static HashSet\<int> ambig_decisions = null; // null = all decisions
    static bool show_profile = false;
    static bool show_tokens = false;
    static bool show_token_count = false;
    static long total_count = 0;
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
    static bool earley = false;
    static int limit = 0; // 0 = unlimited

    static void Main(string[] args)
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
        List\<bool> is_fns = new List\<bool>();
        List\<string> inputs = new List\<string>();
        for (int i = 0; i \< args.Length; ++i)
        {
            if (args[i] == "-d")
            {
                show_diagnostic = true;
            }
            else if (args[i] == "-ambig" || args[i].StartsWith("--ambig"))
            {
                show_ambig = true;
                if (args[i].StartsWith("-ambig=") || args[i].StartsWith("--ambig="))
                {
                    ambig_decisions = new HashSet\<int>();
                    var equal_p = args[i].Split('=');
                    if (equal_p.Count() > 2) throw new Exception("more than one '='-sign in 'ambig' option");
                    var comma_i = equal_p[1].IndexOf(',');
                    if (comma_i > 1) {
                        var comma_p = equal_p[1].Split(',');
                        foreach (var part in comma_p)
                        {
                            if (int.TryParse(part.Trim(), out int d))
                            ambig_decisions.Add(d);
                        }
                }
                else {
                    if (int.TryParse(equal_p[1].Trim(), out int d))
                        ambig_decisions.Add(d);
                    }
                }
            }
            else if (args[i] == "-profile")
            {
                show_profile = true;
            }
            else if (args[i] == "-tokens")
            {
                show_tokens = true;
            }
            else if (args[i] == "-tc")
            {
                show_token_count = true;
            }
            else if (args[i] == "-two-byte")
            {
                two_byte = true;
            }
            else if (args[i] == "-old")
            {
                old = true;
            }
            else if (args[i] == "-tree")
            {
                show_tree = true;
            }
            else if (args[i] == "-prefix")
            {
                prefix = args[++i] + " ";
            }
            else if (args[i] == "-input")
            {
                inputs.Add(args[++i]);
                is_fns.Add(false);
            }
            else if (args[i] == "-tee")
            {
                tee = true;
            }
            else if (args[i] == "-encoding")
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
            else if (args[i] == "-earley")
            {
                earley = true;
                show_tree = false;
            }
            else if (args[i] == "--limit" || args[i].StartsWith("--limit="))
            {
                if (args[i].StartsWith("--limit="))
                {
                    int.TryParse(args[i].Substring(8), out limit);
                }
                else if (i + 1 \< args.Length)
                {
                    int.TryParse(args[++i], out limit);
                }
            }
            else if (args[i][0] == '-')
            {
                // Ignore unknown option.
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
            if (!quiet) System.Console.Error.WriteLine(prefix + "Total Time: " + (after - before).TotalSeconds);
            if (show_token_count) System.Console.Error.WriteLine("TC: " + total_count);
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
        else if (file_encoding == null || file_encoding == "")
        {
            var detected = CharsetDetector.DetectFromFile(input);
            var enc = detected.Detected?.Encoding ?? Encoding.UTF8;
            str = CharStreams.fromPath(input, enc);
        }
        else {
            var encoding = Encoding.GetEncoding(
                file_encoding,
                new EncoderReplacementFallback("(unknown)"),
                new DecoderReplacementFallback("(error)"));
            if (encoding == null)
                throw new Exception(@"Unknown encoding. Must be an Internet Assigned Numbers Authority (IANA) code page name. https://www.iana.org/assignments/character-sets/character-sets.xhtml");
            str = CharStreams.fromPath(input, encoding);
        }
        DoParse(str, input, row_number);
    }

    static void DoParse(ICharStream str, string input_name, int row_number)
    {
        if (binary) str = new BinaryCharStream(str);
        var lexer = new <lexer_name>(str);
        if (show_tokens || show_token_count)
        {
            StringBuilder new_s = new StringBuilder();
            int i = 0;
            for (i = 0; ; ++i)
            {
                var ro_token = lexer.NextToken();
                var token = (CommonToken)ro_token;
                token.TokenIndex = i;
                if (show_tokens) new_s.AppendLine(token.ToString());
                if (token.Type == Antlr4.Runtime.TokenConstants.EOF)
                    break;
            }
            if (show_tokens) System.Console.Error.WriteLine(new_s.ToString());
            total_count += i;
            if (show_tokens) System.Console.Error.WriteLine(new_s.ToString());
            lexer.Reset();
        }
        if (show_token_count) {
            return;
        }
        CommonTokenStream tokens = null;
        if (show_hit) {
            tokens = new ProfilingCommonTokenStream(lexer);
        }
        else {
            tokens = new CommonTokenStream(lexer);
        }
        var parser = new MyParser(tokens);
        var output = tee ? new StreamWriter(input_name + ".errors") : System.Console.Error;
        var listener_lexer = new ErrorListener\<int>(quiet, tee, output);
        var listener_parser = new ErrorListener\<IToken>(quiet, tee, output);
        lexer.RemoveErrorListeners();
        parser.RemoveErrorListeners();
        lexer.AddErrorListener(listener_lexer);
        parser.AddErrorListener(listener_parser);
        if (show_diagnostic)
        {
            parser.AddErrorListener(new MyDiagnosticErrorListener());
        }
        if (show_profile || show_ambig)
        {
            parser.Profile = true;
        }
        if (show_trace)
        {
            parser.Trace = true;
//            ParserATNSimulator.trace_atn_sim = true;
        }
        DateTime before = DateTime.Now;
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
            if (tee)
            {
                System.IO.File.WriteAllText(input_name + ".tree", tree.ToStringTree(parser));
            } else {
                System.Console.Error.WriteLine(tree.ToStringTree(parser));
            }
        }
        if (show_profile)
        {
            System.Console.Error.WriteLine(String.Join(",\n\r", parser.ParseInfo.getDecisionInfo().Select(d => d.ToString())));
        }
        if (show_ambig)
        {
            var decs = parser.ParseInfo.getDecisionInfo().Where(d =>
                d.ambiguities.Any()).Select(d => d.ambiguities).ToList();
            foreach (var decision in decs)
            {
                foreach (var ai in decision)
                {
                    var parser_decision = ai.decision;
                    if (ambig_decisions != null && !ambig_decisions.Contains(parser_decision))
                        continue;
                    var parser_alts = ai.ambigAlts;
                    var parser_startIndex = ai.startIndex;
                    var parser_stopIndex = ai.stopIndex;
                    var nfa_state = parser.Atn.states.Where(s =>
                    {
                        if (s is BasicBlockStartState s2) return s2.decision == parser_decision;
                        else return false;
                    }).ToList();
                    var p = parser.RuleNames.Select((value, index) => new { value, index })
                            .Where(pair => (pair.value == "<start_symbol>"))
                            .Select(pair => pair.index).First();
                    var parser_startRuleIndex = p;
                    var parse_trees = parser.getAllPossibleParseTrees(
                        parser_decision,
                        parser_alts,
                        parser_startIndex,
                        parser_stopIndex,
                        parser_startRuleIndex);
                    foreach (var tuple in parse_trees)
                    {
                        System.Console.WriteLine((ambig_decisions == null || ambig_decisions.Count() > 1 ? (tuple.Item1 + " ") : "") + tuple.Item2.ToStringTree(parser));
                    }
                }
            }
        }
        if (!quiet)
        {
            System.Console.Error.WriteLine(prefix + "CSharp " + row_number + " " + input_name + " " + result + " " + (after - before).TotalSeconds);
        }

        if (earley) {
            var epsilon_remover = new EpsilonRemover(parser);
            var new_atn = epsilon_remover.Convert_ENFA_to_NFA();

            lexer.Reset();
            parser.Reset();

            int start = parser.RuleNames.Select((value, index) => new { value, index })
                .Where(pair => (pair.value == StartSymbol))
                .Select(pair => pair.index).First();

            MyATN atn = new_atn;
            DateTime ebefore = DateTime.Now;
            var accepted = EarleyATN.EarleyAtnRecognizer.ParseToTree(parser, new_atn, parser.TokenStream, start);
            DateTime eafter = DateTime.Now;
            Console.Error.WriteLine((accepted ? "ACCEPT" : "REJECT") + " " + (eafter - ebefore).TotalSeconds);
        }

        if (tee) output.Close();
    }

    public static string ToStringTree(ITree tree, Parser recog)
    {
        StringBuilder sb = new StringBuilder();
        string[] ruleNames = recog != null ? recog.RuleNames : null;
        IList\<string> ruleNamesList = ruleNames != null ? ruleNames.ToList() : null;
        ToStringTree(sb, tree, 0, ruleNamesList);
        return sb.ToString();
    }

    public static void ToStringTree(StringBuilder sb, ITree t, int indent, IList\<string> ruleNames)
    {
        string s = Antlr4.Runtime.Misc.Utils.EscapeWhitespace(GetNodeText(t, ruleNames), false);
        if (t.ChildCount == 0)
        {
            for (int i = 0; i \< indent; ++i) sb.Append(" ");
            sb.AppendLine(s);
            return;
        }
        s = Antlr4.Runtime.Misc.Utils.EscapeWhitespace(GetNodeText(t, ruleNames), false);
        for (int i = 0; i \< indent; ++i) sb.Append(' ');
        sb.AppendLine(s);
        for (int i = 0; i \< t.ChildCount; i++)
        {
            ToStringTree(sb, t.GetChild(i), indent+1, ruleNames);
        }
    }

    public static string GetNodeText(ITree t, Parser recog)
    {
        string[] ruleNames = recog != null ? recog.RuleNames : null;
        IList\<string> ruleNamesList = ruleNames != null ? ruleNames.ToList() : null;
        return GetNodeText(t, ruleNamesList);
    }

    public static string GetNodeText(ITree t, IList\<string> ruleNames)
    {
        if (ruleNames != null)
        {
            if (t is RuleContext)
            {
                int ruleIndex = ((RuleContext)t).RuleIndex;
                string ruleName = ruleNames[ruleIndex];
                int altNumber = ((RuleContext)t).getAltNumber();
                if ( altNumber!= Antlr4.Runtime.Atn.ATN.INVALID_ALT_NUMBER ) {
                    return ruleName+":"+altNumber;
                }
                return ruleName;
            }
            else
            {
                if (t is IErrorNode)
                {
                    return t.ToString();
                }
                else
                {
                    if (t is ITerminalNode)
                    {
                        IToken symbol = ((ITerminalNode)t).Symbol;
                        if (symbol != null)
                        {
                            string s = symbol.Text;
                            return s;
                        }
                    }
                }
            }
        }
        // no recog for rule names
        object payload = t.Payload;
        if (payload is IToken)
        {
            return ((IToken)payload).Text;
        }
        return t.Payload.ToString();
    }
}

<if(has_name_space)>}<endif>