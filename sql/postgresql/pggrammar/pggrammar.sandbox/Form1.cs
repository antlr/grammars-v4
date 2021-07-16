using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using System;
using System.Data;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Xml;


namespace pgcodeanalyze.sandbox
{
    public partial class Form1 : Form
    {

        internal bool UnparsedFound;
        public Form1()
        {
            InitializeComponent();
        }

        private void tbParse_Click(object sender, EventArgs e)
        {
            DoParse(edSource.Text);
        }

        private void toolStripButton1_Click(object sender, EventArgs e)
        {
            DoParse(File.ReadAllText(edFileName.Text));
        }

        private void DoParse(string sql)
        {
            UnparsedFound = false;
            edMessages.Text = "";
            pggrammar ph = pggrammar.getpggrammar(sql);
            ph.AddErrorListener(new SandboxErrorListener() { form = this }); ;
            IParseTree tree = ph.root();
            StreamWriterPostgreListener listener = new StreamWriterPostgreListener();
            listener.Init();
            ParseTreeWalker.Default.Walk(listener, tree);
            listener.wr.Flush();
            edResult.Text = listener.sb.ToString();
            tc.SelectedTab = tabPage2;
            Text = UnparsedFound ? "Found Unparsed" : "OK";

        }
        private void DoParsePlSql(string sql)
        {
            UnparsedFound = false;
            edMessages.Text = "";
            pggrammar ph = pggrammar.getpggrammar(sql);
            ph.AddErrorListener(new SandboxErrorListener() { form = this }); ;
            IParseTree tree = ph.plsqlroot();
            StreamWriterPostgreListener listener = new StreamWriterPostgreListener();
            listener.Init();
            ParseTreeWalker.Default.Walk(listener, tree);
            listener.wr.Flush();
            edResult.Text = listener.sb.ToString();
            tc.SelectedTab = tabPage2;
            Text = UnparsedFound ? "Found Unparsed" : "OK";

        }

        private void toolStripButton2_Click(object sender, EventArgs e)
        {
            DoParsePlSql(edSource.Text);
        }

        private void toolStripButton3_Click(object sender, EventArgs e)
        {
            //
            var r = "";
            var CharStream = CharStreams.fromString(edSource.Text); ;
            var Lexer = new pglexer(CharStream);
            while (true)
            {
                var itoken = Lexer.NextToken();
                if (itoken.Type == -1) break;
                var ruleName = Lexer.Vocabulary.GetDisplayName(itoken.Type);
                r += $"{itoken.Type} {ruleName} {itoken.Text}\r\n";
            }
            edResult.Text = r;
        }

        private void toolStripButton4_Click(object sender, EventArgs e)
        {
            //https://stackoverflow.com/questions/51001389/starting-point-for-antlr4-performance
            Stopwatch sw = new Stopwatch();
            string text = edSource.Text;

            sw.Start();
            pggrammar ph = pggrammar.getpggrammar(text);
            ph.Profile = true;
            sw.Stop();
            Console.WriteLine($"new ph {sw.Elapsed}");
            sw.Start();
            ph.AddErrorListener(new SandboxErrorListener());
            IParseTree tree = ph.root();
            sw.Stop();
            var decisions = ph.ParseInfo.getDecisionInfo().Where(r => r.timeInPrediction > 1000)
                .Select(r => r).OrderByDescending(r => r.timeInPrediction);
            var result = "";
            foreach (var d in decisions)
            {
                var rule = ph.RuleNames[ph.Atn.GetDecisionState(d.decision).ruleIndex];
                result += $"{rule} : {d.timeInPrediction}\r\n";
            }
            edResult.Text = result;

        }


        private pglexer Lexer;


        public string read_sql_construct(int until, int until2, int until3, string expected, string sqlstart,
            bool isexpression,
            bool valid_sql,
            bool trim,
            int? startloc,
            int? endtoken)
        {
            int startlocation = -1;
            int parenlevel = 0;
            var result = sqlstart + "";
            while (true)
            {
                var token = this.Lexer.NextToken();
                if(startlocation<0) startlocation= token.StartIndex;
                if(token.Type==until&&parenlevel==0) break;
                if (token.Type == until2 && parenlevel == 0) break;
                if (token.Type == until3 && parenlevel == 0) break;
                switch (token.Type)
                {
                    case pglexer.OPEN_PAREN:
                    case pglexer.OPEN_BRACKET:
                        parenlevel++;
                        break;
                    case pglexer.CLOSE_PAREN:
                    case pglexer.CLOSE_BRACKET:
                        parenlevel--;
                        if (parenlevel < 0) throw new Exception("mismatched parentheses");
                        break;
                    case pglexer.SEMI:
                    case -1:
                        if (parenlevel != 0) throw new Exception("mismatched parentheses");
                        if (isexpression)
                            throw new SyntaxErrorException($"missing {expected} at end of SQL expression");
                        else
                        {
                            throw new SyntaxErrorException($"missing {expected} at end of SQL statement");
                        }
                }

                result += token.Text;
            }
            if (trim) result = result.TrimEnd();
            if (valid_sql) check_sql_expr(result);
            return result;
        }

        private void check_sql_expr(string result)
        {
            throw new NotImplementedException();
        }


        public  string read_sql_expression(int until, string expected)
        {
            return read_sql_construct(until, 0, 0, expected, "SELECT ", true, true, true, null, null);
        }


        private void toolStripButton5_Click(object sender, EventArgs e)
        {

            var CharStream = CharStreams.fromString(edSource.Text); ;
            Lexer = new pglexer(CharStream);
            edResult.Text = read_sql_expression(pglexer.THEN,"THEN");
        }
    }

    internal class SandboxErrorListener : BaseErrorListener
    {
        internal Form1 form;
        public SandboxErrorListener()
        {
        }
        public override void SyntaxError(TextWriter output, IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
        {
            form.UnparsedFound = true;
            form.edMessages.AppendText($"{line}:{charPositionInLine} {offendingSymbol} {msg}\r\n");
           
        }
    }


    public class StreamWriterPostgreListener : pggrammarBaseListener
    {
        internal int indent = 1;
        public StringBuilder sb = new StringBuilder(16384);
        public XmlWriter wr;  
        public void Init()
        { 
             wr = XmlWriter.Create(sb,new XmlWriterSettings() { Indent = true });
        }
        public override void EnterEveryRule([NotNull] ParserRuleContext context)
        {
            indent++;
            var b = context.GetType().Name;
            wr.WriteStartElement(b);
            if (context is pggrammar.Func_asContext)
            {
                var def = (ParserRuleContext) (context as pggrammar.Func_asContext).Definition;
                if (null != def)
                {
                    ParseTreeWalker.Default.Walk(this, def);
                }
            }
        }
        public override void ExitEveryRule([NotNull] ParserRuleContext context)
        {
            wr.WriteEndElement();
            indent--;
        }

        public override void VisitTerminal([NotNull] ITerminalNode node)
        {
            wr.WriteElementString(node.GetType().Name,node.Symbol.Text);
        }

    }
}
