// ignore_for_file: non_constant_identifier_names, file_names

import 'package:antlr4/antlr4.dart';

abstract class Dart2LexerBase extends Lexer {
  Dart2LexerBase(CharStream input) : super(input);

  bool CheckNotOpenBrace() => inputStream.LA(1) != "{".codeUnitAt(0);
}
