import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';
import 'package:stack/stack.dart';
import 'DartLexer.dart';

abstract class DartParserBase extends Parser
{
    DartParserBase(TokenStream input) : super(input)
    {
    asyncEtcAreKeywords.push(false);
    }

//    String filePath = null;
//    bool ErrorHasOccurred = false;

  /// Must be invoked before the first error is reported for a library.
  /// Will print the name of the library and indicate that it has errors.
//    static void prepareForErrors() {
//	ErrorHasOccurred = true;
//	System.err.println("Syntax error in " + filePath + ":");
//    }

  /// Parse library, return true if success, false if errors occurred.
//    public boolean parseLibrary(String filePath) throws RecognitionException {
//	this.filePath = filePath;
//	errorHasOccurred = false;
//	libraryDefinition();
//	return !errorHasOccurred;
//    }

  // Enable the parser to treat AWAIT/YIELD as keywords in the body of an
  // `async`, `async*`, or `sync*` function. Access via methods below.
    Stack<bool> asyncEtcAreKeywords = new Stack<bool>();

  // Use this to indicate that we are now entering an `async`, `async*`,
  // or `sync*` function.
    void startAsyncFunction() { asyncEtcAreKeywords.push(true); }

  // Use this to indicate that we are now entering a function which is
  // neither `async`, `async*`, nor `sync*`.
    void startNonAsyncFunction() { asyncEtcAreKeywords.push(false); }

  // Use this to indicate that we are now leaving any funciton.
    void endFunction() { asyncEtcAreKeywords.pop(); }

  // Whether we can recognize AWAIT/YIELD as an identifier/typeIdentifier.
    bool asyncEtcPredicate(int tokenId) {
	if (tokenId == DartLexer.TOKEN_AWAIT || tokenId == DartLexer.TOKEN_YIELD) {
	    return !asyncEtcAreKeywords.top();
	}
	return false;
    }

    bool pred1()
    {
	return asyncEtcPredicate(currentToken.type);
    }
}
