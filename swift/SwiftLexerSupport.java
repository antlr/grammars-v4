public class SwiftLexerSupport {

	/**
	 "If an operator has whitespace around both sides or around neither side,
	 it is treated as a binary operator. As an example, the + operator in a+b
	  and a + b is treated as a binary operator."
	*/
	public boolean isBinaryOp() {
		return true;
	}

	/**
	 "If an operator has whitespace on the left side only, it is treated as a
	 prefix unary operator. As an example, the ++ operator in a ++b is treated
	 as a prefix unary operator."
	*/
	public boolean isPrefixOp() {
		return true;
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
	public boolean isPostfixOp() {
		return true;
	}

	public boolean isAndAnd() {
		return true;
	}

	public boolean isOrOr() {
		return true;
	}

	public boolean isArrow() {
		return true;
	}

	public boolean isRange() {
		return true;
	}
}
