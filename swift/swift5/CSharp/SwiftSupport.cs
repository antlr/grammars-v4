using System;
using System.Collections;
using System.IO;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;

namespace SwiftParseTree;

public abstract class SwiftSupport : Parser
{

    protected ITokenStream _input;

    protected SwiftSupport(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        _input = TokenStream;
    }
    /* TODO
    There is one caveat to the rules above. If the ! or ? predefined operator
     has no whitespace on the left, it is treated as a postfix operator,
     regardless of whether it has whitespace on the right. To use the ? as
     the optional-chaining operator, it must not have whitespace on the left.
      To use it in the ternary conditional (? :) operator, it must have
      whitespace around both sides.
    */

    /*
operator-head : /  =  -  +  !  *  %  <  >  &  |  ^  ~  ?
  | [\u00A1-\u00A7]
  | [\u00A9\u00AB]
  | [\u00AC\u00AE]
  | [\u00B0-\u00B1\u00B6\u00BB\u00BF\u00D7\u00F7]
  | [\u2016-\u2017\u2020-\u2027]
  | [\u2030-\u203E]
  | [\u2041-\u2053]
  | [\u2055-\u205E]
  | [\u2190-\u23FF]
  | [\u2500-\u2775]
  | [\u2794-\u2BFF]
  | [\u2E00-\u2E7F]
  | [\u3001-\u3003]
  | [\u3008-\u3030]
  ;
 */
    public static BitArray operatorHead = new BitArray(0x10000);
    public static BitArray operatorCharacter;

    public static BitArray leftWS = new BitArray(255);
    public static BitArray rightWS = new BitArray(255);

    static SwiftSupport()
    {
        // operator-head → /  =­  -­  +­  !­  *­  %­  <­  >­  &­  |­  ^­  ~­  ?­
        operatorHead.Set('/', true);
        operatorHead.Set('=', true);
        operatorHead.Set('-', true);
        operatorHead.Set('+', true);
        operatorHead.Set('!', true);
        operatorHead.Set('*', true);
        operatorHead.Set('%', true);
        operatorHead.Set('<', true);
        operatorHead.Set('>', true);
        operatorHead.Set('&', true);
        operatorHead.Set('|', true);
        operatorHead.Set('^', true);
        operatorHead.Set('~', true);
        operatorHead.Set('?', true);

        // operator-head → U+00A1–U+00A7
        _setBitRange(ref operatorHead, 0x00A1, 0x00A7);

        // operator-head → U+00A9 or U+00AB
        operatorHead.Set((0x00A9), true);
        operatorHead.Set((0x00AB), true);

        // operator-head → U+00AC or U+00AE
        operatorHead.Set((0x00AC), true);
        operatorHead.Set((0x00AE), true);

        // operator-head → U+00A9 or U+00AB
        operatorHead.Set((0x00A9), true);
        operatorHead.Set((0x00AB), true);

        // operator-head → U+00B0–U+00B1, U+00B6, U+00BB, U+00BF, U+00D7, or U+00F7

        operatorHead.Set(0x00B0, true);
        operatorHead.Set(0x00B1, true);
        operatorHead.Set((0x00B6), true);
        operatorHead.Set((0x00BB), true);
        operatorHead.Set((0x00BF), true);
        operatorHead.Set((0x00D7), true);
        operatorHead.Set((0x00F7), true);

        // operator-head → U+2016–U+2017 or U+2020–U+2027
        operatorHead.Set(0x2016, true);
        operatorHead.Set(0x2017, true);
        _setBitRange(ref operatorHead, 0x2020, 0x2027);

        // operator-head → U+2030–U+203E
        _setBitRange(ref operatorHead, 0x2030, 0x203E);

        // operator-head → U+2041–U+2053
        _setBitRange(ref operatorHead, 0x2041, 0x2053);

        // operator-head → U+2055–U+205E
        _setBitRange(ref operatorHead, 0x2055, 0x205E);

        // operator-head → U+2190–U+23FF
        _setBitRange(ref operatorHead, 0x2190, 0x23FF);

        // operator-head → U+2500–U+2775
        _setBitRange(ref operatorHead, 0x2500, 0x2775);

        // operator-head → U+2794–U+2BFF
        _setBitRange(ref operatorHead, 0x2794, 0x2BFF);

        // operator-head → U+2E00–U+2E7F
        _setBitRange(ref operatorHead, 0x2E00, 0x2E7F);

        // operator-head → U+3001–U+3003
        _setBitRange(ref operatorHead, 0x3001, 0x3003);

        // operator-head → U+3008–U+3030
        _setBitRange(ref operatorHead, 0x3008, 0x3030);

        // operator-character → operator-head­
        operatorCharacter = (BitArray)operatorHead.Clone();

        // operator-character → U+0300–U+036F
        _setBitRange(ref operatorCharacter, 0x0300, 0x036F);

        // operator-character → U+1DC0–U+1DFF
        _setBitRange(ref operatorCharacter, 0x1DC0, 0x1DFF);

        // operator-character → U+20D0–U+20FF
        _setBitRange(ref operatorCharacter, 0x20D0, 0x20FF);

        // operator-character → U+FE00–U+FE0F
        _setBitRange(ref operatorCharacter, 0xFE00, 0xFE0F);

        // operator-character → U+FE20–U+FE2F
        _setBitRange(ref operatorCharacter, 0xFE20, 0xFE2F);

        // operator-character → U+E0100–U+E01EF
        // Java works with 16-bit unicode chars. However, it can work for targets in other languages, e.g. in Swift
        // operatorCharacter.set(0xE0100,0xE01EF+1);

        leftWS.Set(Swift5Parser.WS, true);
        leftWS.Set(Swift5Parser.LPAREN, true);
        leftWS.Set(Swift5Parser.LBRACK, true);
        leftWS.Set(Swift5Parser.LCURLY, true);
        leftWS.Set(Swift5Parser.COMMA, true);
        leftWS.Set(Swift5Parser.COLON, true);
        leftWS.Set(Swift5Parser.SEMI, true);
        leftWS.Set(Swift5Parser.Interpolation_multi_line, true);
        leftWS.Set(Swift5Parser.Interpolation_single_line, true);

        rightWS.Set(Swift5Parser.WS, true);
        rightWS.Set(Swift5Parser.RPAREN, true);
        rightWS.Set(Swift5Parser.RBRACK, true);
        rightWS.Set(Swift5Parser.RCURLY, true);
        rightWS.Set(Swift5Parser.COMMA, true);
        rightWS.Set(Swift5Parser.COLON, true);
        rightWS.Set(Swift5Parser.SEMI, true);
        rightWS.Set(Swift5Parser.Line_comment, true);
        rightWS.Set(Swift5Parser.Block_comment, true);
    }

    private static void _setBitRange(ref BitArray bitArray, int firstIndex, int lastIndex)
    {
        for (var i = firstIndex; i < lastIndex; i++)
        {
            bitArray.Set(i, true);
        }
    }

    private static bool _IsCharacterFromSet(IToken token, BitArray bitArray)
    {
        if (token.Type == Eof)
        {
            return false;
        }

        String text = token.Text;
        int codePoint = text[0];

        //  Determine number of characters needed to represent the codePoint character
        if ((codePoint >= 0x10000 ? 2 : 1) != text.Length)
        {
            // not a single character
            return false;
        }

        return operatorCharacter.Get(codePoint);
    }

    public bool isOperatorHead(IToken token)
    {
        return _IsCharacterFromSet(token, operatorHead);
    }

    public bool isOperatorCharacter(IToken token)
    {
        return _IsCharacterFromSet(token, operatorCharacter);
    }

    public bool isOpNext(ITokenStream tokens)
    {
        int start = tokens.Index;
        IToken lt = tokens.Get(start);
        int stop = getLastOpTokenIndex(tokens);
        return stop != -1;
        // System.out.printf("isOpNext: i=%d t='%s'", start, lt.getText());
        // System.out.printf(", op='%s'\n", tokens.getText(Interval.of(start,stop)));
    }

    /** Find stop token index of next operator; return -1 if not operator. */
    public int getLastOpTokenIndex(ITokenStream tokens)
    {
        fillUp(tokens);
        int currentTokenIndex = tokens.Index; // current on-channel lookahead token index
        IToken currentToken = tokens.Get(currentTokenIndex);

        //System.out.println("getLastOpTokenIndex: "+currentToken.getText());


        // operator → dot-operator-head­ dot-operator-characters
        if (currentToken.Type == Swift5Parser.DOT && tokens.Get(currentTokenIndex + 1).Type == Swift5Parser.DOT)
        {
            //System.out.println("DOT");

            // dot-operator
            currentTokenIndex += 2; // point at token after ".."
            currentToken = tokens.Get(currentTokenIndex);

            // dot-operator-character → .­ | operator-character­
            while (currentToken.Type == Swift5Parser.DOT || isOperatorCharacter(currentToken))
            {
                //System.out.println("DOT");
                currentTokenIndex++;
                currentToken = tokens.Get(currentTokenIndex);
            }

            //System.out.println("result: "+(currentTokenIndex - 1));
            return currentTokenIndex - 1;
        }

        // operator → operator-head­ operator-characters­?

        if (isOperatorHead(currentToken))
        {
            //System.out.println("isOperatorHead");

            tokens.GetText(); // TODO. This line strangely fixes crash at mvn test, however, mvn compile gives me perfect working binary.
            currentToken = tokens.Get(currentTokenIndex);
            while (isOperatorCharacter(currentToken))
            {
                //System.out.println("isOperatorCharacter");
                currentTokenIndex++;
                currentToken = tokens.Get(currentTokenIndex);
            }

            //System.out.println("result: "+(currentTokenIndex - 1));
            return currentTokenIndex - 1;
        }
        else
        {
            //System.out.println("result: "+(-1));
            return -1;
        }
    }

    /**
 "If an operator has whitespace around both sides or around neither side,
 it is treated as a binary operator. As an example, the + operator in a+b
 and a + b is treated as a binary operator."
 */
    public bool isBinaryOp(ITokenStream tokens)
    {
        fillUp(tokens);
        int stop = getLastOpTokenIndex(tokens);
        if (stop == -1)
        {
            return false;
        }

        int start = tokens.Index;
        IToken currentToken = tokens.Get(start);
        IToken prevToken = tokens.Get(start - 1); // includes hidden-channel tokens
        IToken nextToken = tokens.Get(stop + 1);
        bool prevIsWS = isLeftOperatorWS(prevToken);
        bool nextIsWS = isRightOperatorWS(nextToken);

        if (currentToken.getType() == Swift5Lexer.QUESTION && start==stop)
        {
            return false;
        }
        
        if (prevIsWS)
        {
            return nextIsWS;
        }

        if (currentToken.getType() == Swift5Lexer.BANG || currentToken.getType() == Swift5Lexer.QUESTION)
        {
            return false;
        }

        if (!nextIsWS) return nextToken.getType() != Swift5Lexer.DOT;

        return false;
    }

    /**
 "If an operator has whitespace on the left side only, it is treated as a
 prefix unary operator. As an example, the ++ operator in a ++b is treated
 as a prefix unary operator."
*/
    public bool isPrefixOp(ITokenStream tokens)
    {
        fillUp(tokens);
        int stop = getLastOpTokenIndex(tokens);
        if (stop == -1) return false;

        int start = tokens.Index;
        IToken prevToken = tokens.Get(start - 1); // includes hidden-channel tokens
        IToken nextToken = tokens.Get(stop + 1);
        bool prevIsWS = isLeftOperatorWS(prevToken);
        bool nextIsWS = isRightOperatorWS(nextToken);
        return prevIsWS && !nextIsWS;
    }

    /**
 "If an operator has whitespace on the right side only, it is treated as a
 postfix unary operator. As an example, the ++ operator in a++ b is treated
 as a postfix unary operator."

 "If an operator has no whitespace on the left but is followed immediately
 by a dot (.), it is treated as a postfix unary operator. As an example,
 the ++ operator in a++.b is treated as a postfix unary operator (a++ .b
 rather than a ++ .b)."
 */
    public bool isPostfixOp(ITokenStream tokens)
    {
        fillUp(tokens);
        int stop = getLastOpTokenIndex(tokens);
        if (stop == -1) return false;

        int start = tokens.Index;
        IToken prevToken = tokens.Get(start - 1); // includes hidden-channel tokens
        IToken nextToken = tokens.Get(stop + 1);
        bool prevIsWS = isLeftOperatorWS(prevToken);
        bool nextIsWS = isRightOperatorWS(nextToken);
        return !prevIsWS && nextIsWS || !prevIsWS && nextToken.Type == Swift5Parser.DOT;
    }

    public bool isOperator(ITokenStream tokens, String op)
    {
        fillUp(tokens);
        int stop = getLastOpTokenIndex(tokens);
        if (stop == -1) return false;

        int start = tokens.Index;
        String text = tokens.GetText(Interval.Of(start, stop));
        // System.out.println("text: '"+text+"', op: '"+op+"', text.equals(op): '"+text.equals(op)+"'");

        for (int i = 0; i <= stop; i++)
        {
            // System.out.println("token["+i+"] = '"+tokens.getText(Interval.of(i, i))+"'");
        }

        return text.Equals(op);
    }

    public bool isLeftOperatorWS(IToken t)
    {
        return leftWS.Get(t.Type);
    }

    public bool isRightOperatorWS(IToken t)
    {
        return rightWS.Get(t.Type) || t.Type == Eof;
    }

    public bool isSeparatedStatement(ITokenStream tokens, int indexOfPreviousStatement)
    {
        fillUp(tokens);
        //System.out.println("------");
        //System.out.println("indexOfPreviousStatement: " + indexOfPreviousStatement);

        int indexFrom = indexOfPreviousStatement - 1;
        int indexTo = tokens.Index - 1;

        if (indexFrom >= 0)
        {
            // Stupid check for new line and semicolon, can be optimized
            while (indexFrom >= 0 && tokens.Get(indexFrom).Channel == Lexer.Hidden)
            {
                indexFrom--;
            }

            //System.out.println("from: '" + tokens.getText(Interval.of(indexFrom, indexFrom))+"', "+tokens.get(indexFrom));
            //System.out.println("to: '" + tokens.getText(Interval.of(indexTo, indexTo))+"', "+tokens.get(indexTo));
            //System.out.println("in_between: '" + tokens.getText(Interval.of(indexFrom, indexTo)));

            //for (int i = previousIndex; i < currentIndex; i++)
            for (int i = indexTo; i >= indexFrom; i--)
            {
                String t = tokens.Get(i).Text;
                if (t.Contains("\n") || t.Contains(";"))
                {
                    return true;
                }
            }

            return false;
            //String text = tokens.getText(Interval.of(indexFrom, indexTo));
            //return text.contains("\n") || text.contains(";");
        }

        return true;
    }

    private void fillUp(ITokenStream tokens)
    {
        for (int i = 1;; ++i)
        {
            int t = tokens.LA(i);
            if (t == -1) break;
        }
    }
}

public static class TokenStreamExtensions
{
    public static int index(this ITokenStream tokenStream) => tokenStream.Index;
    
    public static IToken get(this ITokenStream tokenStream, int i) => tokenStream.Get(i);
}

public static class TokenExtensions
{
    public static int getType(this IToken token) => token.Type;
}