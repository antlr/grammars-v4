using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using SharpCompress.Common;
using System.Collections;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Diagnostics.Metrics;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Numerics;
using System.Reflection.PortableExecutable;
using System.Security.Policy;
using System.Xml.Linq;
using static System.Runtime.InteropServices.JavaScript.JSType;

public abstract class ScalaLexerBase : Lexer
{
    public ScalaLexerBase(ICharStream input)
            : base(input)
    {
        _input = input;
        newLineEnables.Push(false);
    }

    public ScalaLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
            : base(input, output, errorOutput)
    {
        _input = input;
        newLineEnables.Push(false);
    }

    private ICharStream _input;
    bool first = true;
    BufferedList tokens = null;
    int lastIndex = -1;
    bool inCase = false;
    Stack<bool> newLineEnables { get; set; } = new Stack<bool>();
    HashSet<int> arrowsForCase { get; set; } = new HashSet<int>();

    // See https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html#newline-characters
    // for information on newline characters.
    //
    // This code changes newline tokens to either channel(hidden) or channel(default) depending
    // on the rules for newlines in the Spec.
    //
    //   Scala is a line-oriented language where statements may be terminated by semi-colons or
    //   newlines. A newline in a Scala source text is treated as the special token “nl” if the
    //   three following criteria are satisfied:
    //    1) The token immediately preceding the newline can terminate a statement.
    //    2) The token immediately following the newline can begin a statement.
    //    3) The token appears in a region where newlines are enabled.
    //
    //   The tokens that can terminate a statement are: literals, identifiers and the following
    //   delimiters and reserved words:
    //    this    null    true    false    return    type    <xml-start>
    //    _     )       ]       }
    //
    //   The tokens that can begin a statement are all Scala tokens except the following
    //   delimiters and reserved words:
    //    catch    else    extends    finally    forSome    match
    //    with yield   ,    .    ;    :    =    =>    <-    <:    <%
    //    >:    #    [    )    ]    }
    //
    //   A case token can begin a statement only if followed by a class or object token.
    //   Newlines are enabled in:
    //    1) all of a Scala source file, except for nested regions where newlines are disabled, and
    //    2) the interval between matching { and } brace tokens, except for nested regions
    //    where newlines are disabled.
    //
    //   Newlines are disabled in:
    //    1) the interval between matching (and ) parenthesis tokens, except for nested regions
    //    where newlines are enabled, and
    //    2) the interval between matching [ and ] bracket tokens, except for nested regions where newlines are enabled.
    //    3) The interval between a case token and its matching => token, except for nested regions where newlines are enabled.
    //    4) Any regions analyzed in XML mode.
    //   Note that the brace characters of { ... } escapes in XML and string literals are not
    //   tokens, and therefore do not enclose a region where newlines are enabled.
    //   Normally, only a single nl token is inserted between two consecutive non-newline
    //   tokens which are on different lines, even if there are multiple lines between the
    //   two tokens. However, if two tokens are separated by at least one completely blank
    //   line (i.e a line which contains no printable characters), then two nl tokens are
    //   inserted.
    //
    //   The Scala grammar (given in full here) contains productions where optional nl tokens,
    //   but not semicolons, are accepted.This has the effect that a new line in one of these
    //   positions does not terminate an expression or statement.These positions can be
    //   summarized as follows:
    //   Multiple newline tokens are accepted in the following places(note that a semicolon in place of the newline would be illegal in every one of these cases) :
    //    * between the condition of a conditional expression or while loop and the next following expression,
    //    * between the enumerators of a for-comprehension and the next following expression, and
    //    * after the initial type keyword in a type definition or declaration.
    //   A single new line token is accepted
    //    * in front of an opening brace ‘{’, if that brace is a legal continuation of the current statement or expression,
    //    * after an infix operator, if the first token on the next line can start an expression,
    //    * in front of a parameter clause, and
    //    * after an annotation.

    // Further observations of NL tokenization that the Spec does not describe.
    // * A package declaration is not a "statement". So, all preceeding NL's are off-channel.
    //   However, since the package declaration ends with an identifier, NL's are on-channel
    //   afterward.
    // * Tokens "preceeding" and "succeeding" a particular token as stated in the spec are
    //   not for comments or whitespace. Therefore, you have to skip past these to find the
    //   "preceeding" or "succeeding" tokens.

    // NB: Antlr does not do parser-influenced tokenization. Therefore, it's impossible to
    // simply look at the preceding and succeeding tokens to know whether we are at a
    // Scala statement or not!!

    private static HashSet<int> terminators = new HashSet<int>() {
        ScalaLexer.This, // Here and below, per spec.
        ScalaLexer.Null,
        ScalaLexer.BooleanLiteral,
        ScalaLexer.Return,
        ScalaLexer.TypeKW,
        ScalaLexer.UnderScore,
        ScalaLexer.RParen,
        ScalaLexer.RBracket,
        ScalaLexer.RBrace,

        ScalaLexer.IntegerLiteral, // All literals can end a line.
        ScalaLexer.FloatingPointLiteral,
        ScalaLexer.StringLiteral,
        ScalaLexer.CharacterLiteral,

        ScalaLexer.AlphaId, // All identifiers can end a line.
        ScalaLexer.VarId,
        ScalaLexer.BackTickId,

        ///* Tokens that are part of the operator token */
        //ScalaLexer.OpChar,
        //ScalaLexer.Hash,
        //ScalaLexer.Colon,
        //ScalaLexer.Or,
        ///* Tokens that are part of the operator token */
        //ScalaLexer.Exclamation,
        //ScalaLexer.Plus,
        //ScalaLexer.Minus,
        //ScalaLexer.Tilde,
        //ScalaLexer.Star,
        //ScalaLexer.ViewBound,
        ///* XML Terminators */
        //ScalaLexer.XMLCloseTag,
        //ScalaLexer.XMLAutoClose,
        ///* Custom tokens for interpolated strings */
        //ScalaLexer.DoubleQuoteSingle,
        //ScalaLexer.TripleDoubleQuoteMulti
        ///*SymbolLiteral was removed*/
    };
    
    private static HashSet<int> nonStarters = new HashSet<int>() {
        ScalaLexer.Catch, // Here and below, per spec.
        ScalaLexer.Else,
        ScalaLexer.Extends,
        ScalaLexer.Finally,
        ScalaLexer.ForSome,
        ScalaLexer.MatchKW,
        ScalaLexer.With,
        ScalaLexer.Yield,
        ScalaLexer.Comma,
        ScalaLexer.Dot,
        ScalaLexer.SemiColon,
        ScalaLexer.Colon,
        ScalaLexer.Eq,
        ScalaLexer.Arrow,
        ScalaLexer.Assign,
        ScalaLexer.LowerType,
        ScalaLexer.ViewBound,
        ScalaLexer.UpperType,
        ScalaLexer.Hash,
        ScalaLexer.LBracket,
        ScalaLexer.RParen,
        ScalaLexer.RBracket,
        ScalaLexer.RBrace,
        //ScalaLexer.DoubleQuoteSingle, // Not spec.
        //ScalaLexer.TripleDoubleQuoteMulti // Not spec.

        ScalaLexer.Package,
    };

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
        first = true;
        tokens = null;
        lastIndex = -1;
        inCase = false;
        newLineEnables = new Stack<bool>();
        newLineEnables.Push(false);
        arrowsForCase = new HashSet<int>();
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

    //public override IToken NextToken()
    //{
    //    if (first)
    //    {
    //        List<CommonToken> list;
    //        list = new List<CommonToken>();
    //        int index = 0;
    //        for (; ; )
    //        {
    //            var x = base.NextToken() as CommonToken;
    //            x.TokenIndex = index++;
    //            list.Add(x);
    //            if (x.Type == ScalaLexer.Eof) break;
    //        }
    //        tokens = new BufferedList(list);
    //        // Now go through token list and mark up NL as hidden if required.
    //        for (; ; )
    //        {
    //            var x = tokens.LT(1);
    //            Advance(x);
    //            if (x.Type == ScalaLexer.NL)
    //            {
    //                bool canEmitNLToken_ = canEmitNLToken();
    //                if (canEmitNLToken_)
    //                    x.Channel = TokenConstants.DefaultChannel;
    //                else
    //                    x.Channel = TokenConstants.HiddenChannel;
    //            }
    //            tokens.Advance();
    //            if (x.Type == ScalaLexer.Eof) break;
    //        }
    //        tokens.Reset();
    //        first = false;
    //    }
    //    CommonToken t = tokens.LT(1);
    //    tokens.Advance();
    //    return t;
    //}

    private bool canEmitNLToken()
    {
        IToken lb1 = null;
        for (int j = 1; ; )
        {
            lb1 = tokens.LB(j); // Note, we must check for tokens that are already hidden.
            if (lb1 == null) break;
            if (lb1.Channel == TokenConstants.HiddenChannel)
            {
                j++;
                continue;
            }
            break;
        }
        IToken lt2 = null;
        int k;
        for (k = 2; ;)
        {
            lt2 = tokens.LT(k);
            if (lt2 == null) break;
            if (lt2.Type == ScalaLexer.Eof) break;
            if (lt2.Channel == TokenConstants.HiddenChannel)
            {
                k++;
                continue;
            }
            break;
        }
        IToken lt3 = null;
        for (; ; )
        {
            lt3 = tokens.LT(k);
            if (lt3 == null) break;
            if (lt3.Type == ScalaLexer.Eof) break;
            if (lt3.Channel == TokenConstants.HiddenChannel)
            {
                k++;
                continue;
            }
            break;
        }
        bool afterStatementTerminator =
            lb1 != null && terminators.Contains(lb1.Type)
            ;
        bool beforeStatementStarter = lt2 != null && !nonStarters.Contains(lt2.Type)
            || (lt2?.Type == ScalaLexer.Case
                && (lt3 != null && lt3.Type == ScalaLexer.Class || lt3.Type == ScalaLexer.Object));

        if (inCase)
        {
            return false;
        }

        return (isEnabledRegion() || afterStatementTerminator || beforeStatementStarter);
    }

    private bool isEnabledRegion()
    {
        return !newLineEnables.Any() || newLineEnables.Peek();
    }

    private void Advance(CommonToken t)
    {
        IToken lb1 = null;
        for (int j = 1; ;)
        {
            lb1 = tokens.LB(j);
            if (lb1 == null) break;
            if (lb1.Channel == TokenConstants.HiddenChannel)
            {
                j++;
                continue;
            }
            break;
        }
        IToken lt2 = null;
        int k;
        for (k = 2; ;)
        {
            lt2 = tokens.LT(k);
            if (lt2 == null) break;
            if (lt2.Type == ScalaLexer.Eof) break;
            if (lt2.Channel == TokenConstants.HiddenChannel)
            {
                k++;
                continue;
            }
            break;
        }
        IToken lt3 = null;
        for (; ; )
        {
            lt3 = tokens.LT(k);
            if (lt3 == null) break;
            if (lt3.Type == ScalaLexer.Eof) break;
            if (lt3.Channel == TokenConstants.HiddenChannel)
            {
                k++;
                continue;
            }
            break;
        }
        switch (t.Type)
        {
            //case ScalaLexer.Case:
            //    if (/*lt2.Type != ScalaLexer.Class && */ lt2.Type != ScalaLexer.Object)
            //    {
            //        newLineEnables.Push(true);
            //        inCase = true;
            //    }
            //    break;
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
        else if (current - i < 0)
        {
            return null;
        }
        else return tokens[current - i];
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
