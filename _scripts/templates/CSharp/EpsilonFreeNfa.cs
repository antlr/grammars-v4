using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Trash;

public sealed class EpsilonFreeNfa
{
    public Parser Parser { get; }
    public MyATN Atn { get; }
    public int RuleIndex { get; }
    public HashSet<MyATNState> StartStates { get; }
    public HashSet<MyATNState> AcceptStates { get; }
    public HashSet<MyATNState> AllStates { get; }

    internal EpsilonFreeNfa(Parser parser, MyATN atn, int ruleIndex, HashSet<MyATNState> start,
        HashSet<MyATNState> accept, HashSet<MyATNState> all)
    {
        this.Parser = parser;
        Atn = atn;
        StartStates = start;
        AcceptStates = accept;
        AllStates = all;
    }
}

public class EpsilonRemover
{
    private Parser parser;

    public EpsilonRemover(Parser p)
    {
        parser = p;
    }

    public MyATN Convert_ENFA_to_NFA()
    {
        var nfa = Convert(parser.Atn);
//        System.Console.Error.WriteLine(nfa);
        return nfa;
    }

    public MyATN Convert(ATN sourceAtn)
    {
        if (sourceAtn == null) throw new ArgumentNullException(nameof(sourceAtn));

        var start = new MyATNState[sourceAtn.ruleToStartState.Length];
        var stop = new HashSet<MyATNState>();
        var all_states = new HashSet<MyATNState>();
        for (int ruleIndex = 0; ruleIndex < sourceAtn.ruleToStartState.Length; ++ruleIndex)
        {
            var nid = this.parser.RuleNames[ruleIndex];
            var oss = parser.Atn.ruleToStartState[ruleIndex];
            var all = Closure(oss);
            var state_closure = new Dictionary<ATNState, HashSet<ATNState>>();
            foreach (var state in all) state_closure[state] = EClosure(state);
            var cloneMap = new Dictionary<ATNState, MyATNState>();
            var nall = new HashSet<MyATNState>();
            foreach (var s in all)
            {
                MyATNState clone = Clone(s);
                clone.ruleIndex = ruleIndex;
                cloneMap[s] = clone;
                nall.Add(clone);
            }

            // Add non-epsilon transitions.
            foreach (var state in all)
            {
                if (state is RuleStopState) continue;
                var nstate = cloneMap[state];
                foreach (var c in state_closure[state])
                {
                    int ntr = c.NumberOfTransitions;
                    for (int i = 0; i < ntr; i++)
                    {
                        Transition tr = c.Transition(i);
                        var nc = cloneMap[c];
                        // Make sure it's not a duplicate.
                        switch (tr)
                        {
                            case ActionTransition act:
                            {
                                var x = act;
                                nstate.AddTransition(new MyActionTransition(cloneMap[x.target], act.ruleIndex,
                                    act.actionIndex, act.isCtxDependent));
                                break;
                            }
                            case AtomTransition ato:
                            {
                                var x = ato;
                                nstate.AddTransition(new MyAtomTransition(cloneMap[x.target], x.token));
                                break;
                            }
                            case EpsilonTransition _:
                                break;
                            case RangeTransition r:
                            {
                                nstate.AddTransition(new MyRangeTransition(cloneMap[r.target], r.from, r.to));
                                break;
                            }
                            case RuleTransition rul:
                            {
                                var x = rul;
                                nstate.AddTransition(new MyRuleTransition(cloneMap[x.followState], rul.ruleIndex,
                                    rul.precedence));
                                break;
                            }
                            case SetTransition st:
                            {
                                var setCopy = new IntervalSet();
                                setCopy.AddAll(st.set);
                                nstate.AddTransition(new MySetTransition(cloneMap[st.target], setCopy));
                                break;
                            }
                            case WildcardTransition wc:
                            {
                                nstate.AddTransition(new MyWildcardTransition(cloneMap[wc.target]));
                                break;
                            }
                            case PrecedencePredicateTransition ppt:
                            {
                                var x = ppt;
                                nstate.AddTransition(
                                    new MyPrecedencePredicateTransition(cloneMap[x.target], ppt.precedence));
                                break;
                            }
                            case PredicateTransition pred:
                            {
                                nstate.AddTransition(new MyPredicateTransition(cloneMap[pred.target], pred.ruleIndex,
                                    pred.predIndex, pred.isCtxDependent));
                                break;
                            }
                            default:
                                throw new Exception("Unknown case.");
                        }
                    }
                }
            }
            var noss = cloneMap[oss];
            start[ruleIndex] = noss;
            var reachable = Closure(noss);
            foreach (var cl in all)
            {
                var c = cloneMap[cl];
                var ec = EClosure(cl);
                if (ec.Where(s => s is RuleStopState).Any() && reachable.Contains(c))
                    stop.Add(c);
            }
            foreach (var r in reachable) all_states.Add(r);
        }
        var result = new MyATN(parser, start, stop, all_states);
        return result;
    }

    public static HashSet<ATNState> Closure(ATNState start)
    {
        var visited = new HashSet<ATNState>();
        var work = new Stack<ATNState>();
        work.Push(start);
        while (work.Count > 0)
        {
            var s = work.Pop();
            if (!visited.Add(s)) continue;
            // Stop states seem to have transitions out to
            // other the ATN for other rules! This seems to be a
            // *nasty* optimization to indicate where a rule can continue.
            if (s is RuleStopState)
            {
                continue;
            }
            int n = s.NumberOfTransitions;
            for (int i = 0; i < n; i++)
            {
                var tr = s.Transition(i);
                var rule = tr as RuleTransition;
                if (rule != null)
                    work.Push(rule.followState);
                else
                    work.Push(tr.target);
            }
        }
        return visited;
    }

    public static HashSet<MyATNState> Closure(MyATNState start)
    {
        var visited = new HashSet<MyATNState>();
        var work = new Stack<MyATNState>();
        work.Push(start);
        while (work.Count > 0)
        {
            var s = work.Pop();
            if (!visited.Add(s)) continue;
            // Stop states seem to have transitions out to
            // other the ATN for other rules! This seems to be a
            // *nasty* optimization to indicate where a rule can continue.
            if (s.StateType == MyStateType.RuleStop)
            {
                continue;
            }
            int n = s.NumberOfTransitions;
            for (int i = 0; i < n; i++)
            {
                var tr = s.Transition(i);
                work.Push(tr.target);
            }
        }
        return visited;
    }

    private static HashSet<ATNState> EClosure(ATNState state)
    {
        var closure = new HashSet<ATNState>();
        var stack = new Stack<ATNState>();
        stack.Push(state);
        while (stack.Count > 0)
        {
            var s = stack.Pop();
            if (!closure.Add(s)) continue;
            if (s is RuleStopState) continue;
            int n = s.NumberOfTransitions;
            for (int i = 0; i < n; i++)
            {
                var tr = s.Transition(i);
                if (tr is EpsilonTransition)
                {
                    stack.Push(tr.target);
                }
            }
        }
        return closure;
    }

    private static HashSet<MyATNState> EClosure(MyATNState state)
    {
        var closure = new HashSet<MyATNState>();
        var stack = new Stack<MyATNState>();
        stack.Push(state);
        while (stack.Count > 0)
        {
            var s = stack.Pop();
            if (!closure.Add(s)) continue;
            int n = s.NumberOfTransitions;
            for (int i = 0; i < n; i++)
            {
                var tr = s.Transition(i);
                if (tr is MyEpsilonTransition)
                {
                    stack.Push(tr.target);
                }
            }
        }
        return closure;
    }

    private static MyATNState Clone(ATNState s)
    {
        MyATNState clone = new MyATNState();
        switch (s.StateType)
        {
            case StateType.Basic:
                clone.StateType = MyStateType.Basic;
                break;
            case StateType.BlockEnd:
                clone.StateType = MyStateType.BlockEnd;
                break;
            case StateType.BlockStart:
                clone.StateType = MyStateType.BlockStart;
                break;
            case StateType.InvalidType:
                clone.StateType = MyStateType.InvalidType;
                break;
            case StateType.LoopEnd:
                clone.StateType = MyStateType.LoopEnd;
                break;
            case StateType.PlusBlockStart:
                clone.StateType = MyStateType.PlusBlockStart;
                break;
            case StateType.PlusLoopBack:
                clone.StateType = MyStateType.PlusLoopBack;
                break;
            case StateType.RuleStart:
                clone.StateType = MyStateType.RuleStart;
                break;
            case StateType.RuleStop:
                clone.StateType = MyStateType.RuleStop;
                break;
            case StateType.StarBlockStart:
                clone.StateType = MyStateType.StarBlockStart;
                break;
            case StateType.StarLoopBack:
                clone.StateType = MyStateType.StarLoopBack;
                break;
            case StateType.StarLoopEntry:
                clone.StateType = MyStateType.StarLoopEntry;
                break;
            case StateType.TokenStart:
                clone.StateType = MyStateType.TokenStart;
                break;
            default:
                throw new Exception("Unknown type.");
        }
        clone.stateNumber = s.stateNumber;
        return clone;
    }
}
