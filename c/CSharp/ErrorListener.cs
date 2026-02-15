// Generated from trgen 0.23.32

using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Dfa;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Sharpen;
using System;
using System.IO;

public class ErrorListener<S> : IAntlrErrorListener< S>
{
    public bool had_error;
    bool _quiet;
    bool _tee;
    TextWriter _out;

    public ErrorListener(bool quiet, bool tee, TextWriter @out)
    {
        _quiet = quiet;
        _tee = tee;
        _out = @out;
    }

    public void SyntaxError(TextWriter output, IRecognizer recognizer, S offendingSymbol, int line,
        int col, string msg, RecognitionException e)
    {
        string file_name = "<unknown>";
        int line_adjusted = line;
        
        //Get token stream.
        if (recognizer is Parser)
        {
            var p = recognizer as Parser;
            var ts = p.InputStream;
            var i = ts.Index;
            var z = offendingSymbol;
            var q = z as IToken;
            var sou = q.TokenSource;
            var ts2 = ((CommonTokenStream)(p.InputStream));
            var lxer = sou as CLexer;
            // Search back from offending symbol index to find last LineDirective.
            var ind = q.TokenIndex;
            for (int j = ind; ; j--)
            {
                if (j < 0) break;
                var t = ts2.Get(j);
                if (t == null) break;
                if (t.Type == CLexer.LineDirective)
                {
                    // Found it.
                    var txt = t.Text;
                    var parts = txt.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                    if (parts.Length >= 3)
                    {
                        // Get line number from directive.
                        if (int.TryParse(parts[1], out int dir_line))
                        {
                            // Get line number of directive.
                            var line_directive = t.Line;
                            // Get line difference from line directive.
                            var line_diff = line - line_directive;
                            // Adjust line number.
                            line_adjusted = line_diff + dir_line - 1;
                            file_name = parts[2].Trim();
                        }
                    }
                    break;
                }
            }
        }
        had_error = true;
        if (_tee)
        {
            _out.WriteLine(file_name + " line " + line_adjusted + ", .p " + line + ":" + col + " " + msg);

        }
        if (!_quiet)
        {
            System.Console.Error.WriteLine(file_name + " line " + line_adjusted + ", .p " + line + ":" + col + " " + msg);
        }
    }
}

public class MyDiagnosticErrorListener : DiagnosticErrorListener
{
    public override void ReportAmbiguity​(Parser recognizer, DFA dfa, int startIndex, int stopIndex,
        bool exact, BitSet ambigAlts, ATNConfigSet configs)
    {
        string decisionDescription = GetDecisionDescription(recognizer, dfa);
        string text = ((ITokenStream)recognizer.InputStream).GetText(Interval.Of(startIndex, stopIndex));
        string msg = $"ReportAmbiguity​ d={decisionDescription}, input='{text}'";
        System.Console.WriteLine(msg);
        NewMethod(recognizer, dfa, startIndex, stopIndex, configs);
    }

    private void NewMethod(Parser recognizer, DFA dfa, int startIndex, int stopIndex, ATNConfigSet configs)
    {
        try
        {
            string decisionDescription = GetDecisionDescription(recognizer, dfa);
            string text = ((ITokenStream)recognizer.InputStream).GetText(Interval.Of(startIndex, stopIndex));
            int line = recognizer.CurrentToken.Line;
            int col = recognizer.CurrentToken.Column;
            System.Console.WriteLine(line + ":" + col + " " + decisionDescription + " " + text);
            System.Console.WriteLine(configs);
            foreach (var e in configs.Elements)
            {
                ATNState s = e.state;
                PredictionContext c = e.context;
                System.Console.WriteLine(OutIt(recognizer, e, c));
            }
        }
        catch (RecognitionException e)
        {
            System.Console.WriteLine("catch " + e);
        }
    }

    public override void ReportAttemptingFullContext(Parser recognizer, DFA dfa, int startIndex, int stopIndex, BitSet conflictingAlts,
        ATNConfigSet configs)
    {
        string decisionDescription = GetDecisionDescription(recognizer, dfa);
        string text = ((ITokenStream)recognizer.InputStream).GetText(Interval.Of(startIndex, stopIndex));
        string msg = $"ReportAttemptingFullContext d={decisionDescription}, input='{text}'";
        System.Console.WriteLine(msg);
        NewMethod(recognizer, dfa, startIndex, stopIndex, configs);
    }

    public override void ReportContextSensitivity(Parser recognizer, DFA dfa, int startIndex, int stopIndex, int prediction,
        ATNConfigSet configs)
    {
        string decisionDescription = GetDecisionDescription(recognizer, dfa);
        string text = ((ITokenStream)recognizer.InputStream).GetText(Interval.Of(startIndex, stopIndex));
        string msg = $"ReportContextSensitivity d={decisionDescription}, input='{text}'";
        System.Console.WriteLine(msg);
        NewMethod(recognizer, dfa, startIndex, stopIndex, configs);
    }

    string OutIt(Parser recognizer, ATNConfig c, PredictionContext p)
    {
        if (p == null) return "";
        var str = OutIt(recognizer, null, p.GetParent(0));
        int rs = p.GetReturnState(0);
        if (rs != PredictionContext.EMPTY_RETURN_STATE)
        {
            var a = recognizer.Atn;
            var ss = a.states[rs];
            var riss = ss.ruleIndex;
            if (riss < 0) return "";
            var rnss = recognizer.RuleNames[riss];
            if (str != "") str = str + " -> ";
            str = str + rnss;
            if (c != null)
            {
                var k = c.state.ruleIndex;
                str = str + " -> " + recognizer.RuleNames[k];
            }
            return str;
        }
        return "";
    }
}

