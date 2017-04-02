import org.antlr.v4.runtime.ParserRuleContext;
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
	public static final BitSet operatorHead = new BitSet(0x3100); // costs about 2k

	public static final BitSet leftWS = new BitSet(255);
	public static final BitSet rightWS = new BitSet(255);

	static {
		operatorHead.set(Swift2Parser.BANG);
		operatorHead.set(Swift2Parser.LT);
		operatorHead.set(Swift2Parser.GT);
		operatorHead.set(Swift2Parser.AND);
		operatorHead.set(Swift2Parser.OR);
		operatorHead.set(Swift2Parser.SUB);
		operatorHead.set(Swift2Parser.ADD);
		operatorHead.set(Swift2Parser.MUL);
		operatorHead.set(Swift2Parser.DIV);
		operatorHead.set(Swift2Parser.MOD);
		operatorHead.set(Swift2Parser.EQUAL);
		operatorHead.set(Swift2Parser.CARET);
		operatorHead.set(Swift2Parser.TILDE);
		operatorHead.set(Swift2Parser.QUESTION);
		operatorHead.set(0xA1,0xA7+1);
		operatorHead.set(0xA9,0xAB+1);
		operatorHead.set(0xAC,0xAE+1);
		operatorHead.set(0xB0,0xB1+1);
		operatorHead.set(0xB6);
		operatorHead.set(0xBB);
		operatorHead.set(0xBF);
		operatorHead.set(0xD7);
		operatorHead.set(0xF7);
		operatorHead.set(0x2016,0x2017+1);
		operatorHead.set(0x2020,0x2027+1);
		operatorHead.set(0x2030,0x203E+1);
		operatorHead.set(0x2041,0x2053+1);
		operatorHead.set(0x2055,0x205E+1);
		operatorHead.set(0x2190,0x23FF+1);
		operatorHead.set(0x2500,0x2775+1);
		operatorHead.set(0x2794,0x2BFF+1);
		operatorHead.set(0x2E00,0x2E7F+1);
		operatorHead.set(0x3001,0x3003+1);
		operatorHead.set(0x3008,0x3030+1);

		leftWS.set(Swift2Parser.WS);
		leftWS.set(Swift2Parser.LPAREN);
		leftWS.set(Swift2Parser.LBRACK);
		leftWS.set(Swift2Parser.LCURLY);
		leftWS.set(Swift2Parser.COMMA);
		leftWS.set(Swift2Parser.COLON);
		leftWS.set(Swift2Parser.SEMI);

		rightWS.set(Swift2Parser.WS);
		rightWS.set(Swift2Parser.RPAREN);
		rightWS.set(Swift2Parser.RBRACK);
		rightWS.set(Swift2Parser.RCURLY);
		rightWS.set(Swift2Parser.COMMA);
		rightWS.set(Swift2Parser.COLON);
		rightWS.set(Swift2Parser.SEMI);
		rightWS.set(Swift2Parser.Line_comment);
		rightWS.set(Swift2Parser.Block_comment);
	}

	public static boolean isOperatorHead(int ttype) {
		return operatorHead.get(ttype);
	}

	/*
	Operator_character
	  : Operator_head
	  | [\u0300–\u036F]
	  | [\u1DC0–\u1DFF]
	  | [\u20D0–\u20FF]
	  | [\uFE00–\uFE0F]
	  | [\uFE20–\uFE2F]
	  //| [\uE0100–\uE01EF]  ANTLR can't do >16bit char
	  ;
	 */
	public static boolean isOperatorChar(int ttype) {
		return
			operatorHead.get(ttype) ||
			ttype>=0x0300 && ttype<=0x036F ||
			ttype>=0x1DC0 && ttype<=0x1DFF ||
			ttype>=0x20D0 && ttype<=0x20FF ||
			ttype>=0xFE00 && ttype<=0xFE0F ||
			ttype>=0xFE20 && ttype<=0xFE2F;
	}

	public static boolean isOpNext(TokenStream tokens) {
		int start = tokens.index();
		Token lt = tokens.get(start);
		int stop = getLastOpTokenIndex(tokens);
		if ( stop==-1 ) return false;
		System.out.printf("isOpNext: i=%d t='%s'", start, lt.getText());
		System.out.printf(", op='%s'\n", tokens.getText(Interval.of(start,stop)));
		return true;
	}

	/** Find stop token index of next operator; return -1 if not operator. */
	public static int getLastOpTokenIndex(TokenStream tokens) {
		int i = tokens.index(); // current on-channel lookahead token index
		Token lt = tokens.get(i);
		if ( lt.getType()==Swift2Parser.DOT && tokens.get(i+1).getType()==Swift2Parser.DOT ) {
			// dot-operator
			i+=2; // point at token after ".."
			lt = tokens.get(i);
			while ( lt.getType()!=Token.EOF &&
				    (lt.getType()==Swift2Parser.DOT || isOperatorChar(lt.getType())) )
			{
				i++;
				lt = tokens.get(i);
			}
			int stop = i-1;
			return stop;
		}
		// Is it regular operator?
		if ( !isOperatorHead(lt.getType()) ) {
			return -1;
		}
		i++;
		lt = tokens.get(i);
		while ( lt.getType()!=Token.EOF && isOperatorChar(lt.getType()) ) {
			i++;
			lt = tokens.get(i);
		}
		int stop = i-1;
		return stop;
	}

	/**
	 "If an operator has whitespace around both sides or around neither side,
	 it is treated as a binary operator. As an example, the + operator in a+b
	 and a + b is treated as a binary operator."
	 */
	public static boolean isBinaryOp(TokenStream tokens) {
		int stop = getLastOpTokenIndex(tokens);
		if ( stop==-1 ) return false;

		int start = tokens.index();
		Token prevToken = tokens.get(start-1); // includes hidden-channel tokens
		Token nextToken = tokens.get(stop+1);
		boolean prevIsWS = isLeftOperatorWS(prevToken);
		boolean nextIsWS = isRightOperatorWS(nextToken);
		boolean result = prevIsWS && nextIsWS || (!prevIsWS && !nextIsWS);
		String text = tokens.getText(Interval.of(start, stop));
		//System.out.println("isBinaryOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result);
		return result;
	}

	/**
	 "If an operator has whitespace on the left side only, it is treated as a
	 prefix unary operator. As an example, the ++ operator in a ++b is treated
	 as a prefix unary operator."
	*/
	public static boolean isPrefixOp(TokenStream tokens) {
		int stop = getLastOpTokenIndex(tokens);
		if ( stop==-1 ) return false;

		int start = tokens.index();
		Token prevToken = tokens.get(start-1); // includes hidden-channel tokens
		Token nextToken = tokens.get(stop+1);
		boolean prevIsWS = isLeftOperatorWS(prevToken);
		boolean nextIsWS = isRightOperatorWS(nextToken);
		boolean result = prevIsWS && !nextIsWS;
		String text = tokens.getText(Interval.of(start, stop));
		//System.out.println("isPrefixOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result);
		return result;
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
	public static boolean isPostfixOp(TokenStream tokens) {
		int stop = getLastOpTokenIndex(tokens);
		if ( stop==-1 ) return false;

		int start = tokens.index();
		Token prevToken = tokens.get(start-1); // includes hidden-channel tokens
		Token nextToken = tokens.get(stop+1);
		boolean prevIsWS = isLeftOperatorWS(prevToken);
		boolean nextIsWS = isRightOperatorWS(nextToken);
		boolean result =
			!prevIsWS && nextIsWS ||
			!prevIsWS && nextToken.getType()==Swift2Parser.DOT;
		String text = tokens.getText(Interval.of(start, stop));
		//System.out.println("isPostfixOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result);
		return result;
	}

	public static boolean isOperator(TokenStream tokens, String op) {
		int stop = getLastOpTokenIndex(tokens);
		if ( stop==-1 ) return false;

		int start = tokens.index();
		String text = tokens.getText(Interval.of(start, stop));
		return text.equals(op);
	}

	/** Return two booleans packed into lowest 2 bits for left (high) and right (low)
	 *  whitespace.
	 */
	public static int getLeftRightWS(TokenStream tokens, ParserRuleContext ctx) {
		int left = ctx.start.getTokenIndex();
		int right = ctx.stop.getTokenIndex();
		Token prevToken = tokens.get(left-1); // includes hidden-channel tokens
		Token nextToken = tokens.get(right+1);
		boolean prevIsWS = isLeftOperatorWS(prevToken);
		boolean nextIsWS = isRightOperatorWS(nextToken);
		int b = (prevIsWS ? 1 : 0) << 1 | (nextIsWS ? 1 : 0) ;
		return b;
	}

	public static boolean isLeftOperatorWS(Token t) {
		return leftWS.get(t.getType());
	}

	public static boolean isRightOperatorWS(Token t) {
		return rightWS.get(t.getType()) || t.getType()==Token.EOF;
	}
}
