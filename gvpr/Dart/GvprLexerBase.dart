import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'dart:core';
import 'dart:convert';
import 'dart:collection';
import 'gvprLexer.dart';

abstract class GvprLexerBase extends Lexer
{
    GvprLexerBase(CharStream input) : super(input)
    {
    }

    bool IsColumnZero()
    {
        return this.charPositionInLine == 1;
    }
}
