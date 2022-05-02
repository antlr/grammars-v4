import java.util.Stack;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

abstract class DartLexerBase extends Lexer
{
    protected DartLexerBase(CharStream input)
    {
	super(input);
    }

    public static final int BRACE_NORMAL = 1;
    public static final int BRACE_SINGLE = 2;
    public static final int BRACE_DOUBLE = 3;
    public static final int BRACE_THREE_SINGLE = 4;
    public static final int BRACE_THREE_DOUBLE = 5;

  // Enable the parser to handle string interpolations via brace matching.
  // The top of the `braceLevels` stack describes the most recent unmatched
  // '{'. This is needed in order to enable/disable certain lexer rules.
  //
  //   NORMAL: Most recent unmatched '{' was not string literal related.
  //   SINGLE: Most recent unmatched '{' was `'...${`.
  //   DOUBLE: Most recent unmatched '{' was `"...${`.
  //   THREE_SINGLE: Most recent unmatched '{' was `'''...${`.
  //   THREE_DOUBLE: Most recent unmatched '{' was `"""...${`.
  //
  // Access via functions below.
    private Stack<Integer> braceLevels = new Stack<Integer>();

  // Whether we are currently in a string literal context, and which one.
    boolean currentBraceLevel(int braceLevel) {
	if (braceLevels.empty()) return false;
	return braceLevels.peek() == braceLevel;
    }

  // Use this to indicate that we are now entering a specific '{...}'.
  // Call it after accepting the '{'.
    void enterBrace() {
	braceLevels.push(BRACE_NORMAL);
    }
    void enterBraceSingleQuote() {
	braceLevels.push(BRACE_SINGLE);
    }
    void enterBraceDoubleQuote() {
	braceLevels.push(BRACE_DOUBLE);
    }
    void enterBraceThreeSingleQuotes() {
	braceLevels.push(BRACE_THREE_SINGLE);
    }
    void enterBraceThreeDoubleQuotes() {
	braceLevels.push(BRACE_THREE_DOUBLE);
    }

  // Use this to indicate that we are now exiting a specific '{...}',
  // no matter which kind. Call it before accepting the '}'.
    void exitBrace() {
      // We might raise a parse error here if the stack is empty, but the
      // parsing rules should ensure that we get a parse error anyway, and
      // it is not a big problem for the spec parser even if it misinterprets
      // the brace structure of some programs with syntax errors.
	if (!braceLevels.empty()) braceLevels.pop();
    }
    boolean braceLevelSingle()
    {
	return currentBraceLevel(BRACE_SINGLE);
    }

    boolean braceLevelNormal()
    {
	return currentBraceLevel(BRACE_NORMAL);
    }

    boolean braceLevelDouble()
    {
	return currentBraceLevel(BRACE_DOUBLE);
    }

    boolean braceLevelTriple()
    {
	return currentBraceLevel(BRACE_THREE_DOUBLE);
    }

    boolean braceLevelTripleSingle()
    {
	return currentBraceLevel(BRACE_THREE_DOUBLE);
    }
}
