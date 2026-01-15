
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Trash;

public class MyATNState
{
    public const int InitialNumTransitions = 4;

    public static readonly List<string> serializationNames = new List<string>()
    {
        "INVALID", "BASIC", "RULE_START", "BLOCK_START", "PLUS_BLOCK_START", "STAR_BLOCK_START", "TOKEN_START",
        "RULE_STOP", "BLOCK_END", "STAR_LOOP_BACK", "STAR_LOOP_ENTRY", "PLUS_LOOP_BACK", "LOOP_END"
    };

    public const int InvalidStateNumber = -1;

    public ATN atn = null;

    public int stateNumber = InvalidStateNumber;

    public int ruleIndex;

    public bool epsilonOnlyTransitions = false;

    protected internal readonly List<MyTransition> transitions = new List<MyTransition>(InitialNumTransitions);

    protected internal List<MyTransition> optimizedTransitions;

    public IntervalSet nextTokenWithinRule;

    public virtual int NonStopStateNumber
    {
        get
        {
            return stateNumber;
        }
    }

    public override int GetHashCode()
    {
        return stateNumber;
    }

    public override bool Equals(object o)
    {
        return o == this ||
            (o is MyATNState && stateNumber == ((MyATNState)o).stateNumber);
    }

    public virtual bool IsNonGreedyExitState
    {
        get
        {
            return false;
        }
    }

    public override string ToString()
    {
        return stateNumber.ToString();
    }

    public virtual MyTransition[] TransitionsArray
    {
        get
        {
            return transitions.ToArray();
        }
    }

    public virtual int NumberOfTransitions
    {
        get
        {
            return transitions.Count;
        }
    }

    public virtual void AddTransition(MyTransition e)
    {
        AddTransition(transitions.Count, e);
    }

    public virtual void AddTransition(int index, MyTransition e)
    {
        transitions.Insert(index, e);
    }

    public virtual MyTransition Transition(int i)
    {
        return transitions[i];
    }

    public virtual void SetTransition(int i, MyTransition e)
    {
        transitions[i] = e;
    }

    public virtual void RemoveTransition(int index)
    {
        transitions.RemoveAt(index);
    }

    public MyStateType StateType
    {
        get;
        set;
    }

    public bool OnlyHasEpsilonTransitions
    {
        get
        {
            return epsilonOnlyTransitions;
        }
    }

    public virtual void SetRuleIndex(int ruleIndex)
    {
        this.ruleIndex = ruleIndex;
    }

    public virtual bool IsOptimized
    {
        get
        {
            return optimizedTransitions != transitions;
        }
    }

    public virtual int NumberOfOptimizedTransitions
    {
        get
        {
            return optimizedTransitions.Count;
        }
    }

    public virtual MyTransition GetOptimizedTransition(int i)
    {
        return optimizedTransitions[i];
    }

    public virtual void AddOptimizedTransition(MyTransition e)
    {
        if (!IsOptimized)
        {
            optimizedTransitions = new List<MyTransition>();
        }
        optimizedTransitions.Add(e);
    }

    public virtual void SetOptimizedTransition(int i, MyTransition e)
    {
        if (!IsOptimized)
        {
            throw new InvalidOperationException();
        }
        optimizedTransitions[i] = e;
    }

    public virtual void RemoveOptimizedTransition(int i)
    {
        if (!IsOptimized)
        {
            throw new InvalidOperationException();
        }
        optimizedTransitions.RemoveAt(i);
    }

    public MyATNState()
    {
        optimizedTransitions = transitions;
    }

}
