import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';

abstract class PlSqlLexerBase extends Lexer
{
    PlSqlLexerBase(CharStream input) : super(input)
    {
    }

    bool IsNewlineAtPos(int pos)
    {
        int la = inputStream.LA(pos)!;
		if (la == -1) return true;
		return '\n' == String.fromCharCode(inputStream.LA(pos)!);
    }

    bool IsQQuoteDelimiter()
    {
        final opening = inputStream.LA(tokenStartCharIndex + 2 - inputStream.index)!;
        final closing = inputStream.LA(-2)!;
        switch (opening) {
            case 91: return closing == 93;
            case 123: return closing == 125;
            case 60: return closing == 62;
            case 40: return closing == 41;
            default: return closing == opening;
        }
    }
}
