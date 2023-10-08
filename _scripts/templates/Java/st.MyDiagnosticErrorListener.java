// Generated from trgen <version>

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.*;
import org.antlr.v4.runtime.misc.*;
import java.nio.charset.StandardCharsets;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.PrintStream;
import java.util.BitSet;


public class MyDiagnosticErrorListener extends DiagnosticErrorListener
{
    @Override
    public void reportAmbiguity(Parser recognizer, DFA dfa, int startIndex, int stopIndex,
	boolean exact, BitSet ambigAlts, ATNConfigSet configs)
    {
	NewMethod(recognizer, dfa, startIndex, stopIndex, configs);
    }

    private void NewMethod(Parser recognizer, DFA dfa, int startIndex, int stopIndex, ATNConfigSet configs)
    {
	try
	{
	    String decisionDescription = getDecisionDescription(recognizer, dfa);
	    String text = recognizer.getTokenStream().getText(Interval.of(startIndex, stopIndex));
	    int line = recognizer.getCurrentToken().getLine();
	    int col = recognizer.getCurrentToken().getCharPositionInLine();
	    System.out.println(line + ":" + col + " " + decisionDescription + " " + text);
	    System.out.println(configs);
	    for (ATNConfig config : configs.elements()) {
            ATNState s = config.state;
            PredictionContext c = config.context;
            System.out.println(OutIt(recognizer, c));
	    }
	}
	catch (RecognitionException e)
	{
	    System.out.println("catch");
	}
    }

    @Override
    public void reportAttemptingFullContext(Parser recognizer, DFA dfa, int startIndex, int stopIndex, BitSet conflictingAlts,
	ATNConfigSet configs)
    {
	String format = "reportAttemptingFullContext d=%s, input='%s'";
	String decision = getDecisionDescription(recognizer, dfa);
	String text = recognizer.getTokenStream().getText(Interval.of(startIndex, stopIndex));
	String msg = String.format(format, decision, text);
	System.out.println(msg);
	NewMethod(recognizer, dfa, startIndex, stopIndex, configs);
    }

    @Override
    public void reportContextSensitivity(Parser recognizer, DFA dfa, int startIndex, int stopIndex, int prediction,
	ATNConfigSet configs)
    {
	String format = "reportContextSensitivity d=%s, input='%s'";
	String decision = getDecisionDescription(recognizer, dfa);
	String text = recognizer.getTokenStream().getText(Interval.of(startIndex, stopIndex));
	String msg = String.format(format, decision, text);
	System.out.println(msg);
	NewMethod(recognizer, dfa, startIndex, stopIndex, configs);
    }

    String OutIt(Parser recognizer, PredictionContext p)
    {
	if (p == null) return "";
	var str = OutIt(recognizer, p.getParent(0));
	int rs = p.getReturnState(0);
	if (rs != PredictionContext.EMPTY_RETURN_STATE)
	{
	    var a = recognizer.getATN();
	    var ss = a.states.get(rs);
	    var riss = ss.ruleIndex;
	    if (riss \< 0) return "";
	    var rnss = recognizer.getRuleNames()[riss];
	    if (str != "") str = str + " -> ";
	    return str + rnss;
	}
	return "";
    }
}
