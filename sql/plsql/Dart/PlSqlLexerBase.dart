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
}
