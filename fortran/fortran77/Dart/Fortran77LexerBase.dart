import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';

abstract class Fortran77LexerBase extends Lexer
{
    Fortran77LexerBase(CharStream input) : super(input)
    {
    }

    bool IsColumnZero()
    {
        return charPositionInLine == 0;
    }
}
