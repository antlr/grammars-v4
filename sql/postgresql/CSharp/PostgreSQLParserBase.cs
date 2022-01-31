using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

public class PostgreSQLParserBase : Parser
{
    public class ParseError
    {
        public ParseError(int number, int offset, int line, int column, string message)
        {
            Number = number;
            Offset = offset;
            Message = message;
            Line = line;
            Column = column;
        }
        public int Number { get; }
        public int Offset { get; }
        public int Line { get; }
        public int Column { get; }
        public string Message { get; }
    }
    internal readonly IList<ParseError> m_ParseErrors = new List<ParseError>();
    public IList<ParseError> ParseErrors => m_ParseErrors;

    public override string[] RuleNames => throw new System.NotImplementedException();

    public override IVocabulary Vocabulary => throw new System.NotImplementedException();

    public override string GrammarFileName => "PostgreSQLParser.g4";

    public PostgreSQLParserBase(ITokenStream input) : base(input)
    {
    }

    public PostgreSQLParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput) : base(input, output, errorOutput)
    {
    }

    internal IParseTree GetParsedSqlTree(string script, int line = 0)
    {
        var ph = getPostgreSQLParser(script);
        var result = ph.root();
        foreach (var err in ph.ParseErrors)
        {
            ParseErrors.Add(new ParseError(err.Number, err.Offset, err.Line + line, err.Column, err.Message));
        }
        return result;
    }
    internal void ParseRoutineBody(PostgreSQLParser.Createfunc_opt_listContext _localctx)
    {
        var lang =
            _localctx
                .createfunc_opt_item()
                .FirstOrDefault(coi => coi.LANGUAGE() != null)
                ?.nonreservedword_or_sconst()?.nonreservedword()?.identifier()?
                .Identifier()?.GetText();
        var func_as = _localctx.createfunc_opt_item()
            .FirstOrDefault(coi => coi.func_as() != null);
        if (func_as != null)
        {
            var txt = GetRoutineBodyString(func_as.func_as().sconst(0));
            var line = func_as.func_as()
                .sconst(0).Start.Line;
            var ph = getPostgreSQLParser(txt);
            switch (lang)
            {
                case "plpgsql":
                    func_as.func_as().Definition = ph.plsqlroot();
                    break;
                case "sql":
                    func_as.func_as().Definition = ph.root();
                    break;
            }
            foreach (var err in ph.ParseErrors)
            {
                ParseErrors.Add(new ParseError(err.Number, err.Offset, err.Line + line, err.Column, err.Message));
            }
        }
    }
    private static string TrimQuotes(string s)
    {
        return string.IsNullOrEmpty(s) ? s : s.Substring(1, s.Length - 2);
    }

    public static string unquote(string s)
    {
        var r = new StringBuilder(s.Length);
        var i = 0;
        while (i < s.Length)
        {
            var c = s[i];
            r.Append(c);
            if (c == '\'' && i < s.Length - 1 && (s[i + 1] == '\'')) i++;
            i++;
        }
        return r.ToString();
    }
    public static string GetRoutineBodyString(PostgreSQLParser.SconstContext rule)
    {
        var anysconst = rule.anysconst();
        var StringConstant = anysconst.StringConstant();
        if (null != StringConstant) return unquote(TrimQuotes(StringConstant.GetText()));
        var UnicodeEscapeStringConstant = anysconst.UnicodeEscapeStringConstant();
        if (null != UnicodeEscapeStringConstant) return TrimQuotes(UnicodeEscapeStringConstant.GetText());
        var EscapeStringConstant = anysconst.EscapeStringConstant();
        if (null != EscapeStringConstant) return TrimQuotes(EscapeStringConstant.GetText());
        string result = "";
        var dollartext = anysconst.DollarText();
        foreach (var s in dollartext)
        {
            result += s;
        }
        return result;
    }

    public static PostgreSQLParser getPostgreSQLParser(string script)
    {
        var CharStream = CharStreams.fromString(script);
        var Lexer = new PostgreSQLLexer(CharStream);
        var Tokens = new CommonTokenStream(Lexer);
        var Parser = new PostgreSQLParser(Tokens);
        var ErrorListener = new PostgreSQLParserErrorListener();
        ErrorListener.grammar = Parser;
        Parser.AddErrorListener(ErrorListener);
        return Parser;
    }

    internal class PostgreSQLParserErrorListener : BaseErrorListener
    {
        internal PostgreSQLParser grammar;
        public PostgreSQLParserErrorListener()
        {
        }
        public override void SyntaxError(TextWriter output, IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
        {
            grammar?.ParseErrors.Add(new ParseError(0, 0, line, charPositionInLine, msg));
        }
    }



}
