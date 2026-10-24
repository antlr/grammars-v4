import 'dart:io';
import 'package:antlr4/antlr4.dart';
import 'PostgreSQLLexer.dart';

abstract class PostgreSQLLexerBase extends Lexer {
  List<String> tags = [];

  PostgreSQLLexerBase(CharStream input) : super(input);

  void PushTag() {
    tags.add(this.text);
  }

  bool IsTag() {
    return tags.isNotEmpty && this.text == tags.last;
  }

  void PopTag() {
    if (tags.isNotEmpty) {
      tags.removeLast();
    }
  }

  void UnterminatedBlockCommentDebugAssert() {
    assert(inputStream.LA(1) == -1); // EOF
  }

  bool CheckLaMinus() {
    return this.inputStream.LA(1) != '-'.codeUnitAt(0);
  }

  bool CheckLaStar() {
    return this.inputStream.LA(1) != '*'.codeUnitAt(0);
  }

bool isLetter(String ch) {
  if (ch.isEmpty) {
    return false;
  }

  int rune = ch.codeUnitAt(0);
  return (rune >= 0x41 && rune <= 0x5A) || (rune >= 0x61 && rune <= 0x7A);
}

  bool CharIsLetter() {
    return isLetter(String.fromCharCode(inputStream.LA(-1)!));
  }

  void HandleNumericFail() {
    inputStream.seek(this.inputStream.index - 2);
    type = PostgreSQLLexer.TOKEN_Integral;
  }

  void HandleLessLessGreaterGreater() {
    if (text == "<<") type = PostgreSQLLexer.TOKEN_LESS_LESS; // Define PostgreSQLLexer constants as needed
    if (text == ">>") type = PostgreSQLLexer.TOKEN_GREATER_GREATER;
  }

  bool CheckIfUtf32Letter() {
    var char32 = String.fromCharCode(inputStream.LA(-2)!) +
        String.fromCharCode(inputStream.LA(-1)!);
    return isLetter(char32);
  }

  bool IsSemiColon() {
    return ';' == String.fromCharCode(inputStream.LA(1)!);
  }

  bool IsLetter(String char) {
    return RegExp(r'^[a-zA-Z]$').hasMatch(char);
  }
}
