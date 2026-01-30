/* Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
 * Use of this file is governed by the BSD 3-clause license that
 * can be found in the LICENSE.txt file in the project root.
 */

using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;

namespace Trash;

public class MyWildcardTransition : MyTransition
{
    public MyWildcardTransition(MyATNState target)
        : base(target)
    {
    }

    public override bool Matches(int symbol, int minVocabSymbol, int maxVocabSymbol)
    {
        return symbol >= minVocabSymbol && symbol <= maxVocabSymbol;
    }

    [return: NotNull]
    public override string ToString()
    {
        return ".";
    }
}

