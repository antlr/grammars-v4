import java.util.Stack;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

abstract class DartParserBase extends Parser
{
    protected DartParserBase(TokenStream input)
    {
	super(input);
    }

    static String filePath = null;
    static boolean errorHasOccurred = false;

  /// Must be invoked before the first error is reported for a library.
  /// Will print the name of the library and indicate that it has errors.
    static void prepareForErrors() {
	errorHasOccurred = true;
	System.err.println("Syntax error in " + filePath + ":");
    }

  /// Parse library, return true if success, false if errors occurred.
//    public boolean parseLibrary(String filePath) throws RecognitionException {
//	this.filePath = filePath;
//	errorHasOccurred = false;
//	libraryDefinition();
//	return !errorHasOccurred;
//    }

  // Enable the parser to treat AWAIT/YIELD as keywords in the body of an
  // `async`, `async*`, or `sync*` function. Access via methods below.
    private Stack<Boolean> asyncEtcAreKeywords = new Stack<Boolean>();
    { asyncEtcAreKeywords.push(false); }

  // Use this to indicate that we are now entering an `async`, `async*`,
  // or `sync*` function.
    void startAsyncFunction() { asyncEtcAreKeywords.push(true); }

  // Use this to indicate that we are now entering a function which is
  // neither `async`, `async*`, nor `sync*`.
    void startNonAsyncFunction() { asyncEtcAreKeywords.push(false); }

  // Use this to indicate that we are now leaving any funciton.
    void endFunction() { asyncEtcAreKeywords.pop(); }

  // Whether we can recognize AWAIT/YIELD as an identifier/typeIdentifier.
    boolean asyncEtcPredicate(int tokenId) {
	if (tokenId == DartLexer.AWAIT || tokenId == DartLexer.YIELD) {
	    return !asyncEtcAreKeywords.peek();
	}
	return false;
    }

    boolean pred1()
    {
	return  asyncEtcPredicate(getCurrentToken().getType());
    }
}
