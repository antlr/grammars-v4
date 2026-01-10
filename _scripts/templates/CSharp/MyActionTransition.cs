/* Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
 * Use of this file is governed by the BSD 3-clause license that
 * can be found in the LICENSE.txt file in the project root.
 */
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Sharpen;

namespace Trash;

public class MyActionTransition : MyTransition
{
    public readonly int ruleIndex;

    public readonly int actionIndex;

    public readonly bool isCtxDependent;

    public MyActionTransition(MyATNState target, int ruleIndex)
        : this(target, ruleIndex, -1, false)
    {
    }

    public MyActionTransition(MyATNState target, int ruleIndex, int actionIndex, bool isCtxDependent)
        : base(target)
    {
        // e.g., $i ref in action
        this.ruleIndex = ruleIndex;
        this.actionIndex = actionIndex;
        this.isCtxDependent = isCtxDependent;
    }

    // we are to be ignored by analysis 'cept for predicates
    public override bool Matches(int symbol, int minVocabSymbol, int maxVocabSymbol)
    {
        return false;
    }

    public override string ToString()
    {
        return "action_" + ruleIndex + ":" + actionIndex;
    }
}
