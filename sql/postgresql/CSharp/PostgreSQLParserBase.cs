using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using Antlr4.Runtime.Atn;
using System.IO;
using System.Linq;
using System.Text;

public abstract class PostgreSQLParserBase : Parser
{
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
        }
    }

    private string TrimQuotes(string s)
    {
        return string.IsNullOrEmpty(s) ? s : s.Substring(1, s.Length - 2);
    }

    public string unquote(string s)
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

    public string GetRoutineBodyString(PostgreSQLParser.SconstContext rule)
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

    public PostgreSQLParser getPostgreSQLParser(string script)
    {
        var charStream = CharStreams.fromString(script);
        var lexer = new PostgreSQLLexer(charStream);
        var tokens = new CommonTokenStream(lexer);
        var parser = new PostgreSQLParser(tokens);
        lexer.RemoveErrorListeners();
        parser.RemoveErrorListeners();
        var listener_lexer = new LexerDispatchingErrorListener((this.InputStream as CommonTokenStream).TokenSource as Lexer);
        var listener_parser = new ParserDispatchingErrorListener(this);
        lexer.AddErrorListener(listener_lexer);
        parser.AddErrorListener(listener_parser);
        return parser;
    }
}
