using Antlr4.Runtime;
using System.Collections.Generic;
using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using static System.Runtime.InteropServices.JavaScript.JSType;
using System.Diagnostics;
using System.Security.Claims;
using System.Xml.Linq;
using Antlr4.Build.Tasks;
using System.Transactions;
using static Antlr4.Runtime.Atn.SemanticContext;
using static System.Net.WebRequestMethods;
using System.Security.Policy;

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
        base.Reset();
        this.interpolatedStringLevel = 0;
        this.xmlLevels.Clear();
        this.curlyLevels.Clear();
        this.openingTags.Clear();
        this.ready = false;
        ready = false;
        first = true;
        tokens = null;
        lastIndex = -1;
        inCase = false;
        newLineEnables = new Stack<bool>();
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

    bool ready = false;
    bool first = true;
    BufferedList tokens = null;
    int lastIndex = -1;
    bool inCase = false;
    Stack<bool> newLineEnables = new Stack<bool>();
    private static HashSet<int> nonStarters = new HashSet<int>() { ScalaLexer.Catch, ScalaLexer.Else, ScalaLexer.Extends, ScalaLexer.Finally, ScalaLexer.ForSome, ScalaLexer.MatchKW, ScalaLexer.With,
        ScalaLexer.Yield, ScalaLexer.Comma, ScalaLexer.Dot, ScalaLexer.Colon, ScalaLexer.SemiColon, ScalaLexer.Eq, ScalaLexer.Arrow, ScalaLexer.Assign, ScalaLexer.LowerType, ScalaLexer.ViewBound, ScalaLexer.UpperType, ScalaLexer.Hash, ScalaLexer.LBracket,
        ScalaLexer.RParen, ScalaLexer.RBracket, ScalaLexer.RBrace, /* per spec */
        ScalaLexer.DoubleQuoteSingle, ScalaLexer.TripleDoubleQuoteMulti /* custom cases */
    };
    private static HashSet<int> terminators = new HashSet<int>() {
        ScalaLexer.This, ScalaLexer.Null, ScalaLexer.BooleanLiteral, ScalaLexer.Return, ScalaLexer.TypeKW, ScalaLexer.RParen,
            ScalaLexer.RBracket, ScalaLexer.RBrace, ScalaLexer.UnderScore, ScalaLexer.IntegerLiteral, ScalaLexer.FloatingPointLiteral, ScalaLexer.StringLiteral, ScalaLexer.CharacterLiteral,
    /* Id equivalents */
    ScalaLexer.AlphaId, ScalaLexer.VarId, ScalaLexer.BackTickId,
    /* Tokens that are part of the operator token */ ScalaLexer.OpChar, ScalaLexer.Hash, ScalaLexer.Colon, ScalaLexer.Or,
            /* Tokens that are part of the operator token */ ScalaLexer.Exclamation, ScalaLexer.Plus, ScalaLexer.Minus, ScalaLexer.Tilde, ScalaLexer.Star, ScalaLexer.ViewBound,
            /* XML Terminators */ ScalaLexer.XMLCloseTag, ScalaLexer.XMLAutoClose,
            /* Custom tokens for interpolated strings */ ScalaLexer.DoubleQuoteSingle, ScalaLexer.TripleDoubleQuoteMulti
        /*SymbolLiteral was removed*/
    };

    public override IToken NextToken()
    {
        if (first)
        {
            List<CommonToken> list;
            list = new List<CommonToken>();
            int index = 0;
            for (; ; )
            {
                var x = base.NextToken() as CommonToken;
                x.TokenIndex = index++;
                list.Add(x);
                if (x.Type == ScalaLexer.Eof) break;
            }
            tokens = new BufferedList(list);
            // Now go through token list and mark up NL as hidden if required.
            for (; ; )
            {
                var x = tokens.LT(1);
                Advance(x);
                if (x.Type == ScalaLexer.NL)
                {
                    bool canEmitNLToken_ = canEmitNLToken();
                    if (!canEmitNLToken_)
                    {
                        x.Channel = TokenConstants.HiddenChannel;
                    }
                }
                tokens.Advance();
                if (x.Type == ScalaLexer.Eof) break;
            }
            tokens.Reset();
            first = false;
        }
        CommonToken t = tokens.LT(1);
        tokens.Advance();
        return t;
        //if (!ready)
        //{
        //    // Scan ahead for "package", returning everything
        //    // off-channel until then.
        //    if (token.Type == ScalaLexer.NL)
        //    {
        //        var fixed_token = token as CommonToken;
        //        fixed_token.Channel = TokenConstants.HiddenChannel;
        //        return fixed_token;
        //    }
        //    if (token.Type == ScalaLexer.COMMENT)
        //    {
        //        var fixed_token = token as CommonToken;
        //        fixed_token.Channel = TokenConstants.HiddenChannel;
        //        return fixed_token;
        //    }
        //    if (token.Type == ScalaLexer.WS)
        //    {
        //        var fixed_token = token as CommonToken;
        //        fixed_token.Channel = TokenConstants.HiddenChannel;
        //        return fixed_token;
        //    }
        //    ready = true;
        //}
        //// We are past the initial text of the file. Scan ahead to
        //// figure out what to do with NL.
        //if (token.Type == ScalaLexer.NL)
        //{
        //    bool canEmitNLToken_ = canEmitNLToken();
        //    if (!canEmitNLToken_)
        //    {
        //        return token;
        //    }
        //    else return NextToken();
        //}

        //return token;
    }
    bool canEmitNLToken()
    {
        IToken previousToken = tokens.LB(1);
        IToken nextToken = tokens.LT(2);
        bool afterStatementTerminator = previousToken != null && terminators.Contains(previousToken.Type);

        bool beforeStatementStarter = nextToken != null && !nonStarters.Contains(nextToken.Type)
        || (nextToken?.Type == ScalaLexer.Case
                && (tokens.LT(3) != null && tokens.LT(3).Type == ScalaLexer.Class || tokens.LT(3).Type == ScalaLexer.Object));

        if (inCase)
        {
            return false;
        }

        return (isEnabledRegion() && afterStatementTerminator && beforeStatementStarter);
    }

    private bool isEnabledRegion()
    {
        return !newLineEnables.Any() || newLineEnables.Peek();
    }

    private void Advance(CommonToken t)
    {
        switch (t.Type)
        {
            case ScalaLexer.Case:
                if (tokens.LT(2).Type != ScalaLexer.Class && tokens.LT(2).Type != ScalaLexer.Object)
                {
                    newLineEnables.Push(true);
                    inCase = true;
                }
                break;
            case ScalaLexer.Arrow:
                if (inCase)
                {
                    arrowsForCase.Add(t.TokenIndex);
                    newLineEnables.Pop();
                    inCase = false;
                }
                break;
            case ScalaLexer.LBrace:
                newLineEnables.Push(true);
                break;
            case ScalaLexer.LParen:
            case ScalaLexer.LBracket:
                newLineEnables.Push(false);
                break;
            case ScalaLexer.RBrace:
            case ScalaLexer.RParen:
            case ScalaLexer.RBracket:
                newLineEnables.Pop();
                break;
        }
    }

    HashSet<int> arrowsForCase = new HashSet<int>();

    private void StepBack(CommonToken t)
    {
        switch (t.Type)
        {
            case ScalaLexer.Case:
                if (inCase)
                {
                    newLineEnables.Pop();
                    inCase = false;
                }
                break;
            case ScalaLexer.Arrow:
                if (arrowsForCase.Contains(t.TokenIndex))
                {
                    newLineEnables.Push(false);
                    inCase = true;
                }
                break;
            case ScalaLexer.RBrace:
                newLineEnables.Push(true);
                break;
            case ScalaLexer.RParen:
            case ScalaLexer.RBracket:
                newLineEnables.Push(false);
                break;
            case ScalaLexer.LBrace:
            case ScalaLexer.LParen:
            case ScalaLexer.LBracket:
                newLineEnables.Pop();
                break;
        }
        tokens.Backup();
    }
}

public class BufferedList
{
    List<CommonToken> tokens;
    int current = 0;
    public BufferedList(List<CommonToken> list)
    {
        tokens = list;
        current = 0;
    }

    public CommonToken LT(int i)
    {
        Debug.Assert(i > 0);
        if (current + i - 1 >= tokens.Count)
        {
            return tokens[tokens.Count - 1];
        }
        return tokens[current + i - 1];
    }

    public CommonToken LB(int i)
    {
        Debug.Assert(i > 0);
        if (current - i >= tokens.Count)
        {
            return tokens[tokens.Count - 1];
        }
        return tokens[current - i];
    }

    public void Advance()
    {
        current++;
    }

    public void Backup()
    {
        current--;
    }

    internal void Reset()
    {
        current = 0;
    }
}
