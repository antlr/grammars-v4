using System;
using System.Collections.Generic;
using Antlr4.Runtime;
using System.IO;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Sharpen;
using Antlr4.Runtime.Tree;

public class MyParser : algol60Parser {
    private readonly ITokenStream _tokens;
    public MyParserInterpreter _ParserInterpreter;

    public MyParser(ITokenStream input)
        : base(input)
    {
        _tokens = input;
        _ParserInterpreter = new MyParserInterpreter(
            this.Vocabulary,
            this.RuleNames,
            this.Atn,
            input);
    }

    public List<Tuple<string, ParserRuleContext>> getAllPossibleParseTrees(
        int decision,
        BitSet alts,
        int startIndex,
        int stopIndex,
        int startRuleIndex)
    {
        _ParserInterpreter.Interpreter.PredictionMode = PredictionMode.LL_EXACT_AMBIG_DETECTION;
        var trees = new List<Tuple<string,ParserRuleContext>>();

        if ( stopIndex>=(_tokens.Size - 1) ) { // if we are pointing at EOF token
            // EOF is not in tree, so must be 1 less than last non-EOF token
            stopIndex = _tokens.Size - 2;
        }

        // get ambig trees
        int alt = alts.NextSetBit(0);
        while ( alt >= 0 )
        {
            // re-parse entire input for all ambiguous alternatives
            // (don't have to do first as it's been parsed, but do again for simplicity
            //  using this temp parser.)
            _ParserInterpreter.Reset();
            _ParserInterpreter.AddDecisionOverride(decision, startIndex, alt);
            ParserRuleContext t = _ParserInterpreter.Parse(startRuleIndex);
            trees.Add(new Tuple<string, ParserRuleContext>("d=" + decision + ".a=" + alt, t));
            alt = alts.NextSetBit(alt+1);
        }
        return trees;
    }

    public override void RemoveErrorListener(IAntlrErrorListener<IToken> listener)
    {
        base.RemoveErrorListener(listener);
        _ParserInterpreter.RemoveErrorListener(listener);
    }

    public override void RemoveErrorListeners()
    {
        base.RemoveErrorListeners();
        _ParserInterpreter.RemoveErrorListeners();
    }

    public override void AddErrorListener(IAntlrErrorListener<IToken> listener)
    {
        base.AddErrorListener(listener);
        _ParserInterpreter.AddErrorListener(listener);
    }
}
