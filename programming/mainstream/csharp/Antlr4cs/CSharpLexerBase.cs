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

    protected void OnInterpolatedRegularStringStart()
    {
        interpolatedStringLevel++;
        interpolatedVerbatiums.Push(false);
        verbatium = false;
    }

    protected void OnInterpolatedVerbatiumStringStart()
    {
        interpolatedStringLevel++;
        interpolatedVerbatiums.Push(true);
        verbatium = true;
    }

    protected void OnOpenBrace()
    {
        if (interpolatedStringLevel > 0)
        {
            curlyLevels.Push(curlyLevels.Pop() + 1);
        }
    }

    protected void OnCloseBrace()
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

    protected void OnColon()
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

    protected void OpenBraceInside()
    {
        curlyLevels.Push(1);
    }

    protected void OnDoubleQuoteInside()
    {
        interpolatedStringLevel--;
        interpolatedVerbatiums.Pop();
        verbatium = (interpolatedVerbatiums.Count() > 0 ? interpolatedVerbatiums.Peek() : false);
    }

    protected void OnCloseBraceInside()
    {
        curlyLevels.Pop();
    }

    protected bool IsRegularCharInside()
    {
        return !verbatium;
    }

    protected bool IsVerbatiumDoubleQuoteInside()
    {
        return verbatium;
    }
}
