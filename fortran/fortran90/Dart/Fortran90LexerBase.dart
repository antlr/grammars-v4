import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:convert';

abstract class Fortran90LexerBase extends Lexer
{
    Fortran90LexerBase(CharStream input) : super(input)
    {
    }

    bool IsColumnZero()
    {
        return charPositionInLine == 0;
    }
}
