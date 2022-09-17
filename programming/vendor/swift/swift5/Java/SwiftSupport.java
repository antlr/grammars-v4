import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.misc.Interval;

import java.util.BitSet;

public class SwiftSupport {
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
    public static final BitSet operatorHead = new BitSet(0x10000);
    public static final BitSet operatorCharacter;

    public static final BitSet leftWS = new BitSet(255);
    public static final BitSet rightWS = new BitSet(255);

    static {
        // operator-head → /  =­  -­  +­  !­  *­  %­  <­  >­  &­  |­  ^­  ~­  ?­
        operatorHead.set('/');
        operatorHead.set('=');
        operatorHead.set('-');
        operatorHead.set('+');
        operatorHead.set('!');
        operatorHead.set('*');
        operatorHead.set('%');
        operatorHead.set('&');
        operatorHead.set('|');
        operatorHead.set('<');
        operatorHead.set('>');
        operatorHead.set('^');
        operatorHead.set('~');
        operatorHead.set('?');

        // operator-head → U+00A1–U+00A7
        operatorHead.set(0x00A1, 0x00A7 + 1);

        // operator-head → U+00A9 or U+00AB
        operatorHead.set(0x00A9);
        operatorHead.set(0x00AB);

        // operator-head → U+00AC or U+00AE
        operatorHead.set(0x00AC);
        operatorHead.set(0x00AE);

        // operator-head → U+00B0–U+00B1, U+00B6, U+00BB, U+00BF, U+00D7, or U+00F7
        operatorHead.set(0x00B0, 0x00B1 + 1);
        operatorHead.set(0x00B6);
        operatorHead.set(0x00BB);
        operatorHead.set(0x00BF);
        operatorHead.set(0x00D7);
        operatorHead.set(0x00F7);

        // operator-head → U+2016–U+2017 or U+2020–U+2027
        operatorHead.set(0x2016, 0x2017 + 1);
        operatorHead.set(0x2020, 0x2027 + 1);

        // operator-head → U+2030–U+203E
        operatorHead.set(0x2030, 0x203E + 1);

        // operator-head → U+2041–U+2053
        operatorHead.set(0x2041, 0x2053 + 1);

        // operator-head → U+2055–U+205E
        operatorHead.set(0x2055, 0x205E + 1);

        // operator-head → U+2190–U+23FF
        operatorHead.set(0x2190, 0x23FF + 1);

        // operator-head → U+2500–U+2775
        operatorHead.set(0x2500, 0x2775 + 1);

        // operator-head → U+2794–U+2BFF
        operatorHead.set(0x2794, 0x2BFF + 1);

        // operator-head → U+2E00–U+2E7F
        operatorHead.set(0x2E00, 0x2E7F + 1);

        // operator-head → U+3001–U+3003
        operatorHead.set(0x3001, 0x3003 + 1);

        // operator-head → U+3008–U+3030
        operatorHead.set(0x3008, 0x3020 + 1);

        operatorHead.set(0x3030);

        // operator-character → operator-head­
        operatorCharacter = (BitSet) operatorHead.clone();

        // operator-character → U+0300–U+036F
        operatorCharacter.set(0x0300, 0x036F + 1);
        // operator-character → U+1DC0–U+1DFF
        operatorCharacter.set(0x1DC0, 0x1DFF + 1);
        // operator-character → U+20D0–U+20FF
        operatorCharacter.set(0x20D0, 0x20FF + 1);
        // operator-character → U+FE00–U+FE0F
        operatorCharacter.set(0xFE00, 0xFE0F + 1);
        // operator-character → U+FE20–U+FE2F
        operatorCharacter.set(0xFE20, 0xFE2F + 1);
        operatorCharacter.set(0xE0100, 0xE01EF + 1);

        // operator-character → U+E0100–U+E01EF
        // Java works with 16-bit unicode chars. However, it can work for targets in other languages, e.g. in Swift
        // operatorCharacter.set(0xE0100,0xE01EF+1);

        leftWS.set(Swift5Parser.WS);
        leftWS.set(Swift5Parser.LPAREN);
        leftWS.set(Swift5Parser.Interpolataion_multi_line);
        leftWS.set(Swift5Parser.Interpolataion_single_line);
        leftWS.set(Swift5Parser.LBRACK);
        leftWS.set(Swift5Parser.LCURLY);
        leftWS.set(Swift5Parser.COMMA);
        leftWS.set(Swift5Parser.COLON);
        leftWS.set(Swift5Parser.SEMI);

        rightWS.set(Swift5Parser.WS);
        rightWS.set(Swift5Parser.RPAREN);
        rightWS.set(Swift5Parser.RBRACK);
        rightWS.set(Swift5Parser.RCURLY);
        rightWS.set(Swift5Parser.COMMA);
        rightWS.set(Swift5Parser.COLON);
        rightWS.set(Swift5Parser.SEMI);
        rightWS.set(Swift5Parser.Line_comment);
        rightWS.set(Swift5Parser.Block_comment);
    }

    private static boolean isCharacterFromSet(Token token, BitSet bitSet) {
        if (token.getType() == Token.EOF) {
            return false;
        } else {
            String text = token.getText();
            int codepoint = text.codePointAt(0);
            if (Character.charCount(codepoint) != text.length()) {
                // not a single character
                return false;
            } else {
                return bitSet.get(codepoint);
            }
        }
    }

    public static boolean isOperatorHead(Token token) {
        return isCharacterFromSet(token, operatorHead);
    }

    public static boolean isOperatorCharacter(Token token) {
        return isCharacterFromSet(token, operatorCharacter);
    }

    public static boolean isOpNext(TokenStream tokens) {
        int start = tokens.index();
        Token lt = tokens.get(start);
        int stop = getLastOpTokenIndex(tokens);
        return stop != -1;
        // System.out.printf("isOpNext: i=%d t='%s'", start, lt.getText());
        // System.out.printf(", op='%s'\n", tokens.getText(Interval.of(start,stop)));
    }

    /**
     * Find stop token index of next operator; return -1 if not operator.
     */
    public static int getLastOpTokenIndex(TokenStream tokens) {
	SwiftSupport.fillUp(tokens);
        int currentTokenIndex = tokens.index(); // current on-channel lookahead token index
        Token currentToken = tokens.get(currentTokenIndex);

        //System.out.println("getLastOpTokenIndex: "+currentToken.getText());


        // operator → dot-operator-head­ dot-operator-characters
        if (currentToken.getType() == Swift5Parser.DOT && tokens.get(currentTokenIndex + 1).getType() == Swift5Parser.DOT) {
            //System.out.println("DOT");


            // dot-operator
            currentTokenIndex += 2; // point at token after ".."
            currentToken = tokens.get(currentTokenIndex);

            // dot-operator-character → .­ | operator-character­
            while (currentToken.getType() == Swift5Parser.DOT || isOperatorCharacter(currentToken)) {
                //System.out.println("DOT");
                currentTokenIndex++;
                currentToken = tokens.get(currentTokenIndex);
            }

            //System.out.println("result: "+(currentTokenIndex - 1));
            return currentTokenIndex - 1;
        }

        // operator → operator-head­ operator-characters­?

        if (isOperatorHead(currentToken)) {
            //System.out.println("isOperatorHead");

            currentToken = tokens.get(currentTokenIndex);
            while (isOperatorCharacter(currentToken)) {
                //System.out.println("isOperatorCharacter");
                currentTokenIndex++;
                currentToken = tokens.get(currentTokenIndex);
            }
            //System.out.println("result: "+(currentTokenIndex - 1));
            return currentTokenIndex - 1;
        } else {
            //System.out.println("result: "+(-1));
            return -1;
        }
    }

    /**
     * "If an operator has whitespace around both sides or around neither side,
     * it is treated as a binary operator. As an example, the + operator in a+b
     * and a + b is treated as a binary operator."
     */
    public static boolean isBinaryOp(TokenStream tokens) {
	SwiftSupport.fillUp(tokens);
        int stop = getLastOpTokenIndex(tokens);
        if (stop == -1) return false;

        int start = tokens.index();
        Token currentToken = tokens.get(start);
        Token prevToken = tokens.get(start - 1); // includes hidden-channel tokens
        Token nextToken = tokens.get(stop + 1);
        boolean prevIsWS = isLeftOperatorWS(prevToken);
        boolean nextIsWS = isRightOperatorWS(nextToken);
        //String text = tokens.getText(Interval.of(start, stop));
        //System.out.println("isBinaryOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result);
        if (prevIsWS) {
            return nextIsWS;
        } else {
            if (currentToken.getType() == Swift5Lexer.BANG || currentToken.getType() == Swift5Lexer.QUESTION) {
                return false;
            } else {
                if (!nextIsWS) return nextToken.getType() != Swift5Lexer.DOT;
            }
        }
        return false;
    }

    /**
     * "If an operator has whitespace on the left side only, it is treated as a
     * prefix unary operator. As an example, the ++ operator in a ++b is treated
     * as a prefix unary operator."
     */
    public static boolean isPrefixOp(TokenStream tokens) {
	SwiftSupport.fillUp(tokens);
        int stop = getLastOpTokenIndex(tokens);
        if (stop == -1) return false;

        int start = tokens.index();
        Token prevToken = tokens.get(start - 1); // includes hidden-channel tokens
        Token nextToken = tokens.get(stop + 1);
        boolean prevIsWS = isLeftOperatorWS(prevToken);
        boolean nextIsWS = isRightOperatorWS(nextToken);
        //String text = tokens.getText(Interval.of(start, stop));
        // System.out.println("isPrefixOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result);
        return prevIsWS && !nextIsWS;
    }

    /**
     * "If an operator has whitespace on the right side only, it is treated as a
     * postfix unary operator. As an example, the ++ operator in a++ b is treated
     * as a postfix unary operator."
     * <p>
     * "If an operator has no whitespace on the left but is followed immediately
     * by a dot (.), it is treated as a postfix unary operator. As an example,
     * the ++ operator in a++.b is treated as a postfix unary operator (a++ .b
     * rather than a ++ .b)."
     */
    public static boolean isPostfixOp(TokenStream tokens) {
	SwiftSupport.fillUp(tokens);
        int stop = getLastOpTokenIndex(tokens);
        if (stop == -1) return false;

        int start = tokens.index();
        Token prevToken = tokens.get(start - 1); // includes hidden-channel tokens
        Token nextToken = tokens.get(stop + 1);
        boolean prevIsWS = isLeftOperatorWS(prevToken);
        boolean nextIsWS = isRightOperatorWS(nextToken);
        //String text = tokens.getText(Interval.of(start, stop));
        // System.out.println("isPostfixOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result);
        return !prevIsWS && nextIsWS ||
                !prevIsWS && nextToken.getType() == Swift5Parser.DOT;
    }

    public static boolean isOperator(TokenStream tokens, String op) {
	SwiftSupport.fillUp(tokens);
        int stop = getLastOpTokenIndex(tokens);
        if (stop == -1) return false;

        int start = tokens.index();
        String text = tokens.getText(Interval.of(start, stop));
        // System.out.println("text: '"+text+"', op: '"+op+"', text.equals(op): '"+text.equals(op)+"'");

        // for (int i = 0; i <= stop; i++) {
        //     System.out.println("token["+i+"] = '"+tokens.getText(Interval.of(i, i))+"'");
        // }

        return text.equals(op);
    }

    public static boolean isLeftOperatorWS(Token t) {
        return leftWS.get(t.getType());
    }

    public static boolean isRightOperatorWS(Token t) {
        return rightWS.get(t.getType()) || t.getType() == Token.EOF;
    }

    public static boolean isSeparatedStatement(TokenStream tokens, int indexOfPreviousStatement) {
	SwiftSupport.fillUp(tokens);
        //System.out.println("------");
        //System.out.println("indexOfPreviousStatement: " + indexOfPreviousStatement);

        int indexFrom = indexOfPreviousStatement - 1;
        int indexTo = tokens.index() - 1;

        if (indexFrom >= 0) {
            // Stupid check for new line and semicolon, can be optimized
            while (indexFrom >= 0 && tokens.get(indexFrom).getChannel() == Token.HIDDEN_CHANNEL) {
                indexFrom--;
            }

            //System.out.println("from: '" + tokens.getText(Interval.of(indexFrom, indexFrom))+"', "+tokens.get(indexFrom));
            //System.out.println("to: '" + tokens.getText(Interval.of(indexTo, indexTo))+"', "+tokens.get(indexTo));
            //System.out.println("in_between: '" + tokens.getText(Interval.of(indexFrom, indexTo)));

            //for (int i = previousIndex; i < currentIndex; i++)
            for(int i =indexTo;i>= indexFrom;i--){
                String t = tokens.get(i).getText();
                if(t.contains("\n") || t.contains(";")){
                    return true;
                }
            }
            return false;
            //String text = tokens.getText(Interval.of(indexFrom, indexTo));
            //return text.contains("\n") || text.contains(";");
        } else {
            return true;
        }
    }
    public static void fillUp(TokenStream tokens)
    {
	for (int jj = 1;;++jj)
	{
	    int t = tokens.LA(jj);
	    if (t == -1) break;
	}
    }
}
