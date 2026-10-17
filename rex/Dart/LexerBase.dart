import 'package:antlr4/antlr4.dart';

abstract class LexerBase extends Lexer {
  LexerBase(CharStream input) : super(input);

  bool Check1() => inputStream.LA(1) != 58;
}
