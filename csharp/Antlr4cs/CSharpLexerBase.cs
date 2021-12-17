using Antlr4.Runtime;
using System.Collections.Generic;
using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

public abstract class CSharpLexerBase : Lexer
{
    public CSharpLexerBase(ICharStream input)
        : base(input)
    {
    }

    protected int interpolatedStringLevel;
    protected Stack<bool> interpolatedVerbatiums = new Stack<bool>();
    protected Stack<int> curlyLevels = new Stack<int>();
    protected bool verbatium;

    protected void Action1()
    {
        interpolatedStringLevel++;
        interpolatedVerbatiums.Push(false);
        verbatium = false;
    }

    protected void Action2()
    {
        interpolatedStringLevel++;
        interpolatedVerbatiums.Push(true);
        verbatium = true;
    }

    protected void Action3()
    {
        if (interpolatedStringLevel > 0)
        {
            curlyLevels.Push(curlyLevels.Pop() + 1);
        }
    }

    protected void Action4()
    {

        if (interpolatedStringLevel > 0)
        {
            curlyLevels.Push(curlyLevels.Pop() - 1);
            if (curlyLevels.Peek() == 0)
            {
                curlyLevels.Pop();
                Skip();
                PopMode();
            }
        }
    }

    protected void Action5()
    {

        if (interpolatedStringLevel > 0)
        {
            int ind = 1;
            bool switchToFormatString = true;
            while ((char)_input.La(ind) != '}')
            {
                if (_input.La(ind) == ':' || _input.La(ind) == ')')
                {
                    switchToFormatString = false;
                    break;
                }
                ind++;
            }
            if (switchToFormatString)
            {
                this.Mode(Test.CSharpLexer.INTERPOLATION_FORMAT);
            }
        }
    }

    protected void Action6()
    {
        curlyLevels.Push(1);
    }

    protected void Action7()
    {
        interpolatedStringLevel--;
        interpolatedVerbatiums.Pop();
        verbatium = (interpolatedVerbatiums.Count() > 0 ? interpolatedVerbatiums.Peek() : false);
    }

    protected void Action8()
    {
        curlyLevels.Pop();
    }

    protected bool Pred1()
    {
        return !verbatium;
    }

    protected bool Pred2()
    {
        return verbatium;
    }
}
