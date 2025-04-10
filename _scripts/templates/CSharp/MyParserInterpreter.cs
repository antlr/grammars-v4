/* Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
 * Use of this file is governed by the BSD 3-clause license that
 * can be found in the LICENSE.txt file in the project root.
 */
using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Dfa;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Sharpen;

/// <summary>
/// A parser simulator that mimics what ANTLR's generated
/// parser code does.
/// </summary>
/// <remarks>
/// A parser simulator that mimics what ANTLR's generated
/// parser code does. A ParserATNSimulator is used to make
/// predictions via adaptivePredict but this class moves a pointer through the
/// ATN to simulate parsing. ParserATNSimulator just
/// makes us efficient rather than having to backtrack, for example.
/// This properly creates parse trees even for left recursive rules.
/// We rely on the left recursive rule invocation and special predicate
/// transitions to make left recursive rules work.
/// See TestParserInterpreter for examples.
/// </remarks>
public class MyParserInterpreter : Parser
{
    private readonly ATN _atn;

    private readonly DFA[] _decisionToDFA;

    protected internal readonly BitSet pushRecursionContextStates;

    private readonly string[] _ruleNames;

    [NotNull]
    private readonly IVocabulary vocabulary;

    private Stack<Tuple<ParserRuleContext, int>> _parentContextStack = new Stack<Tuple<ParserRuleContext, int>>();

    /** We need a map from (decision,inputIndex)->forced alt for computing ambiguous
     *  parse trees. For now, we allow exactly one override.
     */
    protected int overrideDecision = -1;
    protected int overrideDecisionInputIndex = -1;
    protected int overrideDecisionAlt = -1;
    protected bool overrideDecisionReached = false; // latch and only override once; error might trigger infinite loop

    /** What is the current context when we override a decisions?  This tells
     *  us what the root of the parse tree is when using override
     *  for an ambiguity/lookahead check.
     */
    protected InterpreterRuleContext overrideDecisionRoot = null;
    protected InterpreterRuleContext rootContext;

    public MyParserInterpreter(IVocabulary vocabulary, IEnumerable<string> ruleNames, ATN atn, ITokenStream input)
        : base(input)
    {
        this._atn = atn;
        this._ruleNames = ruleNames.ToArray();
        this.vocabulary = vocabulary;
        // identify the ATN states where pushNewRecursionContext must be called
        this.pushRecursionContextStates = new BitSet(atn.states.Count);
        foreach (ATNState state in atn.states)
        {
            if (!(state is StarLoopEntryState))
            {
                continue;
            }
            if (((StarLoopEntryState)state).isPrecedenceDecision)
            {
                this.pushRecursionContextStates.Set(state.stateNumber);
            }
        }

        //init decision DFA
        int numberofDecisions = atn.NumberOfDecisions;
        this._decisionToDFA = new DFA[numberofDecisions];
        for (int i = 0; i < numberofDecisions; i++)
        {
            DecisionState decisionState = atn.GetDecisionState(i);
            _decisionToDFA[i] = new DFA(decisionState, i);
        }
         // get atn simulator that knows how to do predictions
        Interpreter = new ParserATNSimulator(this, atn, _decisionToDFA, null);
    }

    public override void Reset() {
        base.Reset();
        overrideDecisionReached = false;
        overrideDecisionRoot = null;
        overrideDecision = -1;
        overrideDecisionInputIndex = -1;
        overrideDecisionAlt = -1;
        _parentContextStack = new Stack<Tuple<ParserRuleContext, int>>();
        overrideDecisionRoot = null;
        rootContext = null;
    }

    public override ATN Atn
    {
        get
        {
            return _atn;
        }
    }

    public override IVocabulary Vocabulary
    {
        get
        {
            return vocabulary;
        }
    }

    public override string[] RuleNames
    {
        get
        {
            return _ruleNames;
        }
    }

    /// <summary>Begin parsing at startRuleIndex</summary>
    public virtual ParserRuleContext Parse(int startRuleIndex)
    {
        RuleStartState startRuleStartState = _atn.ruleToStartState[startRuleIndex];
        InterpreterRuleContext rootContext = new InterpreterRuleContext(null, ATNState.InvalidStateNumber, startRuleIndex);
        if (startRuleStartState.isPrecedenceRule)
        {
            EnterRecursionRule(rootContext, startRuleStartState.stateNumber, startRuleIndex, 0);
        }
        else
        {
            EnterRule(rootContext, startRuleStartState.stateNumber, startRuleIndex);
        }
        while (true)
        {
            ATNState p = AtnState;
            switch (p.StateType)
            {
                case StateType.RuleStop:
                {
                    // pop; return from rule
                    if (RuleContext.IsEmpty)
                    {
                        if (startRuleStartState.isPrecedenceRule)
                        {
                            ParserRuleContext result = RuleContext;
                            Tuple<ParserRuleContext, int> parentContext = _parentContextStack.Pop();
                            UnrollRecursionContexts(parentContext.Item1);
                            return result;
                        }
                        else
                        {
                            ExitRule();
                            return rootContext;
                        }
                    }
                    VisitRuleStopState(p);
                    break;
                }

                default:
                {
                    try
                    {
                        VisitState(p);
                    }
                    catch (RecognitionException e)
                    {
                        State = _atn.ruleToStopState[p.ruleIndex].stateNumber;
                        Context.exception = e;
                        ErrorHandler.ReportError(this, e);
                        ErrorHandler.Recover(this, e);
                    }
                    break;
                }
            }
        }
    }

    public override void EnterRecursionRule(ParserRuleContext localctx, int state, int ruleIndex, int precedence)
    {
        _parentContextStack.Push(Tuple.Create(RuleContext, localctx.invokingState));
        base.EnterRecursionRule(localctx, state, ruleIndex, precedence);
    }

    protected internal virtual ATNState AtnState
    {
        get
        {
            return _atn.states[State];
        }
    }

    public override string GrammarFileName => throw new NotImplementedException();

    protected internal virtual void VisitState(ATNState p)
    {
        int predictedAlt = 1;
        if ( p is DecisionState ) {
            predictedAlt = visitDecisionState((DecisionState) p);
        }

        Transition transition = p.Transition(predictedAlt - 1);
        switch (transition.TransitionType)
        {
            case TransitionType.EPSILON:
            {
                if (pushRecursionContextStates.Get(p.stateNumber) && !(transition.target is LoopEndState))
                {
                    InterpreterRuleContext ctx = new InterpreterRuleContext(_parentContextStack.Peek().Item1, _parentContextStack.Peek().Item2, RuleContext.RuleIndex);
                    PushNewRecursionContext(ctx, _atn.ruleToStartState[p.ruleIndex].stateNumber, RuleContext.RuleIndex);
                }
                break;
            }

            case TransitionType.ATOM:
            {
                Match(((AtomTransition)transition).token);
                break;
            }

            case TransitionType.RANGE:
            case TransitionType.SET:
            case TransitionType.NOT_SET:
            {
                if (!transition.Matches(TokenStream.LA(1), TokenConstants.MinUserTokenType, 65535))
                {
                    ErrorHandler.RecoverInline(this);
                }
                MatchWildcard();
                break;
            }

            case TransitionType.WILDCARD:
            {
                MatchWildcard();
                break;
            }

            case TransitionType.RULE:
            {
                RuleStartState ruleStartState = (RuleStartState)transition.target;
                int ruleIndex = ruleStartState.ruleIndex;
                InterpreterRuleContext ctx_1 = new InterpreterRuleContext(RuleContext, p.stateNumber, ruleIndex);
                if (ruleStartState.isPrecedenceRule)
                {
                    EnterRecursionRule(ctx_1, ruleStartState.stateNumber, ruleIndex, ((RuleTransition)transition).precedence);
                }
                else
                {
                    EnterRule(ctx_1, transition.target.stateNumber, ruleIndex);
                }
                break;
            }

            case TransitionType.PREDICATE:
            {
                PredicateTransition predicateTransition = (PredicateTransition)transition;
                if (!Sempred(RuleContext, predicateTransition.ruleIndex, predicateTransition.predIndex))
                {
                    throw new FailedPredicateException(this);
                }
                break;
            }

            case TransitionType.ACTION:
            {
                ActionTransition actionTransition = (ActionTransition)transition;
                Action(RuleContext, actionTransition.ruleIndex, actionTransition.actionIndex);
                break;
            }

            case TransitionType.PRECEDENCE:
            {
                if (!Precpred(RuleContext, ((PrecedencePredicateTransition)transition).precedence))
                {
                    throw new FailedPredicateException(this, string.Format("precpred(Context, {0})", ((PrecedencePredicateTransition)transition).precedence));
                }
                break;
            }

            default:
            {
                throw new NotSupportedException("Unrecognized ATN transition type.");
            }
        }
        State = transition.target.stateNumber;
    }

    /** Method visitDecisionState() is called when the interpreter reaches
     *  a decision state (instance of DecisionState). It gives an opportunity
     *  for subclasses to track interesting things.
     */
    protected int visitDecisionState(DecisionState p) {
        int predictedAlt = 1;
        if ( p.NumberOfTransitions > 1 ) {
            ErrorHandler.Sync(this);
            int decision = p.decision;
            if ( decision == overrideDecision && InputStream.Index == overrideDecisionInputIndex &&
                 !overrideDecisionReached )
            {
                predictedAlt = overrideDecisionAlt;
                overrideDecisionReached = true;
            }
            else {
                predictedAlt = Interpreter.AdaptivePredict((ITokenStream)InputStream, decision, Context);
            }
        }
        return predictedAlt;
    }

    /** Provide simple "factory" for InterpreterRuleContext's.
     *  @since 4.5.1
     */
    protected InterpreterRuleContext createInterpreterRuleContext(
        ParserRuleContext parent,
        int invokingStateNumber,
        int ruleIndex)
    {
        return new InterpreterRuleContext(parent, invokingStateNumber, ruleIndex);
    }

    protected internal virtual void VisitRuleStopState(ATNState p)
    {
        RuleStartState ruleStartState = _atn.ruleToStartState[p.ruleIndex];
        if (ruleStartState.isPrecedenceRule)
        {
            Tuple<ParserRuleContext, int> parentContext = _parentContextStack.Pop();
            UnrollRecursionContexts(parentContext.Item1);
            State = parentContext.Item2;
        }
        else
        {
            ExitRule();
        }
        RuleTransition ruleTransition = (RuleTransition)_atn.states[State].Transition(0);
        State = ruleTransition.followState.stateNumber;
    }


    /** Override this parser interpreters normal decision-making process
     *  at a particular decision and input token index. Instead of
     *  allowing the adaptive prediction mechanism to choose the
     *  first alternative within a block that leads to a successful parse,
     *  force it to take the alternative, 1..n for n alternatives.
     *
     *  As an implementation limitation right now, you can only specify one
     *  override. This is sufficient to allow construction of different
     *  parse trees for ambiguous input. It means re-parsing the entire input
     *  in general because you're never sure where an ambiguous sequence would
     *  live in the various parse trees. For example, in one interpretation,
     *  an ambiguous input sequence would be matched completely in expression
     *  but in another it could match all the way back to the root.
     *
     *  s : e '!'? ;
     *  e : ID
     *    | ID '!'
     *    ;
     *
     *  Here, x! can be matched as (s (e ID) !) or (s (e ID !)). In the first
     *  case, the ambiguous sequence is fully contained only by the root.
     *  In the second case, the ambiguous sequences fully contained within just
     *  e, as in: (e ID !).
     *
     *  Rather than trying to optimize this and make
     *  some intelligent decisions for optimization purposes, I settled on
     *  just re-parsing the whole input and then using
     *  {link Trees#getRootOfSubtreeEnclosingRegion} to find the minimal
     *  subtree that contains the ambiguous sequence. I originally tried to
     *  record the call stack at the point the parser detected and ambiguity but
     *  left recursive rules create a parse tree stack that does not reflect
     *  the actual call stack. That impedance mismatch was enough to make
     *  it it challenging to restart the parser at a deeply nested rule
     *  invocation.
     *
     *  Only parser interpreters can override decisions so as to avoid inserting
     *  override checking code in the critical ALL(*) prediction execution path.
     *
     *  @since 4.5.1
     */
    public void AddDecisionOverride(int decision, int tokenIndex, int forcedAlt) {
        overrideDecision = decision;
        overrideDecisionInputIndex = tokenIndex;
        overrideDecisionAlt = forcedAlt;
    }

    public InterpreterRuleContext GetOverrideDecisionRoot() {
        return overrideDecisionRoot;
    }

    /** Rely on the error handler for this parser but, if no tokens are consumed
     *  to recover, add an error node. Otherwise, nothing is seen in the parse
     *  tree.
     */
    protected void Recover(RecognitionException e) {
        int i = InputStream.Index;
        ErrorHandler.Recover(this, e);
        if ( InputStream.Index==i ) {
            // no input consumed, better add an error node
            if ( e is InputMismatchException ) {
                InputMismatchException ime = (InputMismatchException)e;
                IToken tok = e.OffendingToken;
                int expectedTokenType = TokenConstants.InvalidType;
                if ( !ime.GetExpectedTokens().IsNil ) {
                    expectedTokenType = ime.GetExpectedTokens().MinElement; // get any element
                }
                IToken errToken = TokenFactory.Create(new Tuple<ITokenSource, ICharStream>(tok.TokenSource, tok.TokenSource.InputStream),
                    expectedTokenType, tok.Text,
                    TokenConstants.DefaultChannel,
                    -1, -1, // invalid start/stop
                    tok.Line, tok.Column);
                Context.AddErrorNode(errToken);
            }
            else { // NoViableAlt
                IToken tok = e.OffendingToken;
                IToken errToken = TokenFactory.Create(new Tuple<ITokenSource, ICharStream>(tok.TokenSource, tok.TokenSource.InputStream),
                    TokenConstants.InvalidType, tok.Text,
                    TokenConstants.DefaultChannel,
                    -1, -1, // invalid start/stop
                    tok.Line, tok.Column);
                Context.AddErrorNode(errToken);
            }
        }
    }

    protected IToken RecoverInline() {
        return ErrorHandler.RecoverInline(this);
    }

    /** Return the root of the parse, which can be useful if the parser
     *  bails out. You still can access the top node. Note that,
     *  because of the way left recursive rules add children, it's possible
     *  that the root will not have any children if the start rule immediately
     *  called and left recursive rule that fails.
     *
     * @since 4.5.1
     */
    public InterpreterRuleContext GetRootContext() {
        return rootContext;
    }
}
