/* Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
 * Use of this file is governed by the BSD 3-clause license that
 * can be found in the LICENSE.txt file in the project root.
 */
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Sharpen;

namespace Trash;

public class MyPrecedencePredicateTransition : MyAbstractPredicateTransition
{
    public readonly int precedence;

    public MyPrecedencePredicateTransition(MyATNState target, int precedence)
        : base(target)
    {
        this.precedence = precedence;
    }

    public override bool Matches(int symbol, int minVocabSymbol, int maxVocabSymbol)
    {
        return false;
    }

    public SemanticContext.PrecedencePredicate Predicate
    {
        get
        {
            return new SemanticContext.PrecedencePredicate(precedence);
        }
    }

    public override string ToString()
    {
        return precedence + " >= _p";
    }
}
