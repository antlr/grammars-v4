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
        return la == -1 || la == '\n';
    }
}
