using Antlr4.Runtime;
using System.Collections.Generic;
using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

public abstract class ScalaLexerBase : Lexer
{
    public ScalaLexerBase(ICharStream input)
            : base(input)
    {
        _input = input;
    }

    public ScalaLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
            : base(input, output, errorOutput)
    {
        _input = input;
    }

    protected int interpolatedStringLevel = 0;
    protected Stack<int> xmlLevels = new Stack<int>();
    protected Stack<int> curlyLevels = new Stack<int>();
    protected Stack<char> openingTags = new Stack<char>();

    public override void Reset()
    {
        this.interpolatedStringLevel = 0;
        this.xmlLevels.Clear();
        this.curlyLevels.Clear();
        this.openingTags.Clear();
        base.Reset();
    }

    protected void popModeForIdentifier()
    {
        if (interpolatedStringLevel > 0
            && (this.ModeStack.Peek() == ScalaLexer.InterpolationStringSingleLine || this.ModeStack.Peek() == ScalaLexer.InterpolationStringMultiLine)
            && curlyLevels.Peek() == 0)
        {
            PopMode();
            curlyLevels.Pop();
        }
    }

    protected void addCurly()
    {
        if (interpolatedStringLevel > 0 || xmlLevels.Count > 0)
        {
            curlyLevels.Push(curlyLevels.Pop() + 1);
            if (curlyLevels.Peek() == 1)
            {
                xmlLevels.Push(0);
            }
        }
    }

    protected void startXMLMode()
    {
        if (xmlLevels.Count == 0)
        {
            xmlLevels.Push(1);
        }
        else if (xmlLevels.Peek() == 0)
        {
            xmlLevels.Push(xmlLevels.Pop() + 1);
        }
        else xmlLevels.Push(1);
        this.openingTags.Push(this.Text[this.Text.Length - 1]);
        this.Type = ScalaLexer.XMLOutsideNode;
        this.Type = ScalaLexer.XMLInsideNode;
    }

    protected bool canOpenInterpolatedString()
    {
        return interpolatedStringLevel == 0 || (curlyLevels.Count > 0 && curlyLevels.Peek() > 0);
    }

    protected void onRBrace()
    {
        if (interpolatedStringLevel > 0 || xmlLevels.Count > 0)
        {
            curlyLevels.Push(curlyLevels.Pop() - 1);
            if (curlyLevels.Peek() == 0)
            {
                if (this.ModeStack.Peek() == ScalaLexer.XMLInsideNode || this.ModeStack.Peek() == ScalaLexer.XMLOutsideNode)
                {
                    this.Type = ScalaLexer.RBraceXML;
                }
                curlyLevels.Pop();
                if (xmlLevels.Peek() == 0)
                {
                    xmlLevels.Pop();
                }
                PopMode();
            }

        }
    }

    private ICharStream _input;
    //protected int interpolatedStringLevel;
    //protected Stack<bool> interpolatedVerbatiums = new Stack<bool>();
    //protected Stack<int> curlyLevels = new Stack<int>();
    //protected bool verbatium;

}
