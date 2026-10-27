import 'dart:convert';
import 'package:antlr4/antlr4.dart';

abstract class PostgreSQLParserBase extends Parser {

  PostgreSQLParserBase(TokenStream input) : super(input);

  void ParseRoutineBody() {
  }

  bool OnlyAcceptableOps() {
    var token = (this.inputStream as CommonTokenStream).LT(1)!;
    var text = token.text;
    return text == '!' || text == '!!' || text == '!=-';
  }
}
