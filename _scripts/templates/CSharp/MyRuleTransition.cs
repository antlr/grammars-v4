/* Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
 * Use of this file is governed by the BSD 3-clause license that
 * can be found in the LICENSE.txt file in the project root.
 */
using System;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;

namespace Trash;

public class MyRuleTransition : MyTransition
{
    /// <summary>Ptr to the rule definition object for this rule ref</summary>
    public readonly int ruleIndex;

    public readonly int precedence;

    public bool tailCall;

    public bool optimizedTailCall;

    public MyRuleTransition(MyATNState target, int ruleIndex, int precedence)
        : base(target)
    {
        // no Rule object at runtime
        this.ruleIndex = ruleIndex;
        this.precedence = precedence;
    }

    public override bool Matches(int symbol, int minVocabSymbol, int maxVocabSymbol)
    {
        return false;
    }
}

