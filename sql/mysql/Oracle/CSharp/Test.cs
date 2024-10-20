// Generated from trgen 0.23.7

using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
public class Program
{
    public static MySQLParser Parser { get; set; }
    public static ErrorListener<IToken> ParserErrorListener { get; set; }
    public static MySQLLexer Lexer { get; set; }
    public static ErrorListener<int> LexerErrorListener { get; set; }
    public static ITokenStream TokenStream { get; set; }
    public static ICharStream CharStream { get; set; }
    public static IParseTree Tree { get; set; }
    public static List<IParseTree> Trees { get; set; }
    public static string StartSymbol { get; set; } = "queries";
    public static string Input { get; set; }
    public static bool HeatMap { get; set; } = false;
    public static void SetupParse2(string input, bool quiet = false)
    {
        ICharStream str = new AntlrInputStream(input);
        CharStream = str;
        var lexer = new MySQLLexer(str);
        Lexer = lexer;
        CommonTokenStream tokens = null;
        if (HeatMap) {
            tokens = new ProfilingCommonTokenStream(lexer);
        }
        else {
            tokens = new CommonTokenStream(lexer);
        }
        TokenStream = tokens;
        var parser = new MyParser(tokens);
        Parser = parser;
        var listener_lexer = new ErrorListener<int>(false, false, System.Console.Error);
        var listener_parser = new ErrorListener<IToken>(false, false, System.Console.Error);
        LexerErrorListener = listener_lexer;
        ParserErrorListener = listener_parser;
        lexer.RemoveErrorListeners();
        parser.RemoveErrorListeners();
        lexer.AddErrorListener(listener_lexer);
        parser.AddErrorListener(listener_parser);
    }

    public static IParseTree Parse2()
    {
        var tree = Parser.queries();
        Input = Lexer.InputStream.ToString();
        TokenStream = Parser.TokenStream;
        Tree = tree;
        return tree;
    }

    public static List<Tuple<int, List<IParseTree>>> Parse3()
    {
        Parser.Profile = true;
        var tree = Parser.queries();
        var decisions = Parser.ParseInfo.getDecisionInfo().Where(d => d.ambiguities.Any()).ToList();
        var result = new List<Tuple<int, List<IParseTree>>>();
        foreach (var decision in decisions)
        {
            var am = decision.ambiguities;
            var trees = new List<IParseTree>();
            foreach (AmbiguityInfo ai in am)
            {
                var parser_decision = ai.decision;
                var parser_alts = ai.ambigAlts;
                var parser_startIndex = ai.startIndex;
                var parser_stopIndex = ai.stopIndex;
                var p = Parser.RuleNames.Select((value, index) => new { value, index })
                        .Where(pair => (pair.value == "queries"))
                        .Select(pair => pair.index).First();
                var parser_startRuleIndex = p;
                var parser_trees = ((MyParser)Parser).getAllPossibleParseTrees(
                    parser_decision,
                    parser_alts,
                    parser_startIndex,
                    parser_stopIndex,
                    parser_startRuleIndex);
                trees.AddRange(parser_trees);
            }
            result.Add(new Tuple<int, List<IParseTree>>(decision.decision, trees));
        }
        Input = Lexer.InputStream.ToString();
        TokenStream = Parser.TokenStream;
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
        var lexer = new MySQLLexer(str);
        Lexer = lexer;
        CommonTokenStream tokens = null;
        if (show_hit) {
            tokens = new ProfilingCommonTokenStream(lexer);
        }
        else {
            tokens = new CommonTokenStream(lexer);
        }
        TokenStream = tokens;
        var parser = new MySQLParser(tokens);
        Parser = parser;
        var listener_lexer = new ErrorListener<int>(false, false, System.Console.Error);
        var listener_parser = new ErrorListener<IToken>(false, false, System.Console.Error);
        lexer.RemoveErrorListeners();
        parser.RemoveErrorListeners();
        lexer.AddErrorListener(listener_lexer);
        parser.AddErrorListener(listener_parser);
        var tree = parser.queries();
        Input = lexer.InputStream.ToString();
        TokenStream = parser.TokenStream;
        Tree = tree;
        return tree;
    }

    static bool tee = false;
    static bool show_diagnostic = false;
    static bool show_hit = false;
    static bool show_ambig = false;
    static bool show_profile = false;
    static bool show_tokens = false;
    static bool show_trace = false;
    static bool show_tree = false;
    static bool old = false;
    static bool two_byte = false;
    static int exit_code = 0;
    static Encoding encoding = null;
    static int string_instance = 0;
    static string prefix = "";
    static bool quiet = false;

    static void Main(string[] args)
    {
        List<bool> is_fns = new List<bool>();
        List<string> inputs = new List<string>();
        for (int i = 0; i < args.Length; ++i)
        {
            if (args[i] == "-d")
            {
                show_diagnostic = true;
            }
            else if (args[i] == "-ambig")
            {
                show_ambig = true;
            }
            else if (args[i] == "-profile")
            {
                show_profile = true;
            }
            else if (args[i] == "-tokens")
            {
                show_tokens = true;
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
            for (int f = 0; f < inputs.Count(); ++f)
            {
                if (is_fns[f])
                    ParseFilename(inputs[f], f);
                else
                    ParseString(inputs[f], f);
            }
            DateTime after = DateTime.Now;
            if (!quiet) System.Console.Error.WriteLine("Total Time: " + (after - before).TotalSeconds);
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
        var lexer = new MySQLLexer(str);
        lexer.serverVersion = 80200;
        lexer.sqlModeFromString("ANSI_QUOTES");
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
        CommonTokenStream tokens = null;
        if (show_hit) {
            tokens = new ProfilingCommonTokenStream(lexer);
        }
        else {
            tokens = new CommonTokenStream(lexer);
        }
        var parser = new MyParser(tokens);
        var output = tee ? new StreamWriter(input_name + ".errors") : System.Console.Error;
        var listener_lexer = new ErrorListener<int>(quiet, tee, output);
        var listener_parser = new ErrorListener<IToken>(quiet, tee, output);
        lexer.RemoveErrorListeners();
        parser.RemoveErrorListeners();
        lexer.AddErrorListener(listener_lexer);
        parser.AddErrorListener(listener_parser);
//lexer.charSets = charSets;
parser.serverVersion = lexer.serverVersion;
parser.sqlModes = lexer.sqlModes;
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
        var tree = parser.queries();
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
                    var parser_alts = ai.ambigAlts;
                    var parser_startIndex = ai.startIndex;
                    var parser_stopIndex = ai.stopIndex;
                    var nfa_state = parser.Atn.states.Where(s =>
                    {
                        if (s is BasicBlockStartState s2) return s2.decision == parser_decision;
                        else return false;
                    }).ToList();
                    var p = parser.RuleNames.Select((value, index) => new { value, index })
                            .Where(pair => (pair.value == "queries"))
                            .Select(pair => pair.index).First();
                    var parser_startRuleIndex = p;
                    var parser_trees = parser.getAllPossibleParseTrees(
                        parser_decision,
                        parser_alts,
                        parser_startIndex,
                        parser_stopIndex,
                        parser_startRuleIndex);
                    foreach (var parser_tree in parser_trees)
                    {
                        System.Console.WriteLine(parser_tree.ToStringTree(parser));
                        System.Console.WriteLine();
                    }
                }
            }
        }
        if (!quiet)
        {
            System.Console.Error.WriteLine(prefix + "CSharp " + row_number + " " + input_name + " " + result + " " + (after - before).TotalSeconds);
        }
        if (tee) output.Close();
    }

    public static string ToStringTree(ITree tree, Parser recog)
    {
        StringBuilder sb = new StringBuilder();
        string[] ruleNames = recog != null ? recog.RuleNames : null;
        IList<string> ruleNamesList = ruleNames != null ? ruleNames.ToList() : null;
        ToStringTree(sb, tree, 0, ruleNamesList);
        return sb.ToString();
    }

    public static void ToStringTree(StringBuilder sb, ITree t, int indent, IList<string> ruleNames)
    {
        string s = Antlr4.Runtime.Misc.Utils.EscapeWhitespace(GetNodeText(t, ruleNames), false);
        if (t.ChildCount == 0)
        {
            for (int i = 0; i < indent; ++i) sb.Append(" ");
            sb.AppendLine(s);
            return;
        }
        s = Antlr4.Runtime.Misc.Utils.EscapeWhitespace(GetNodeText(t, ruleNames), false);
        for (int i = 0; i < indent; ++i) sb.Append(' ');
        sb.AppendLine(s);
        for (int i = 0; i < t.ChildCount; i++)
        {
            ToStringTree(sb, t.GetChild(i), indent+1, ruleNames);
        }
    }

    public static string GetNodeText(ITree t, Parser recog)
    {
        string[] ruleNames = recog != null ? recog.RuleNames : null;
        IList<string> ruleNamesList = ruleNames != null ? ruleNames.ToList() : null;
        return GetNodeText(t, ruleNamesList);
    }

    public static string GetNodeText(ITree t, IList<string> ruleNames)
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

