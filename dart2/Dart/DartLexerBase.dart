import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';
import 'package:stack/stack.dart';
import 'DartLexer.dart';

abstract class DartLexerBase extends Lexer
{
    DartLexerBase(CharStream input) : super(input)
    {
    }

    int BRACE_NORMAL = 1;
    int BRACE_SINGLE = 2;
    int BRACE_DOUBLE = 3;
    int BRACE_THREE_SINGLE = 4;
    int BRACE_THREE_DOUBLE = 5;

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
    Stack<int> braceLevels = new Stack<int>();

  // Whether we are currently in a string literal context, and which one.
    bool currentBraceLevel(int braceLevel) {
	if (braceLevels.isEmpty) return false;
	return braceLevels.top() == braceLevel;
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
	if (!braceLevels.isEmpty) braceLevels.pop();
    }
	bool braceLevelSingle()
	{
		return currentBraceLevel(BRACE_SINGLE);
	}
	bool braceLevelNormal()
	{
		return currentBraceLevel(BRACE_NORMAL);
	}

	bool braceLevelDouble()
	{
		return currentBraceLevel(BRACE_DOUBLE);
	}

	bool braceLevelTriple()
	{
		return currentBraceLevel(BRACE_THREE_DOUBLE);
	}
	
	bool braceLevelTripleSingle()
	{
		return currentBraceLevel(BRACE_THREE_DOUBLE);
	}
}
