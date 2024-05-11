import 'package:antlr4/antlr4.dart';
import 'bnfLexer.dart';

abstract class bnfParserBase extends Parser {
  final TokenStream _input;

  bnfParserBase(this._input)
      : super(_input);

  bool NotNL() {
    int i = 1;
    var c = (_input as CommonTokenStream).LT(i);
    bool v = c?.type != bnfLexer.TOKEN_NL;
    return v;
  }

  bool notAssign() {
    int i = 1;
    var c = (_input as CommonTokenStream).LT(i);
    while (c != null && c.type == bnfLexer.TOKEN_WS) {
      c = (_input as CommonTokenStream).LT(++i);
    }
    bool v = !(c?.type == bnfLexer.TOKEN_ASSIGN1 ||
        c?.type == bnfLexer.TOKEN_ASSIGN2 ||
        c?.type == bnfLexer.TOKEN_ASSIGN3 ||
        c?.type == bnfLexer.TOKEN_ASSIGN4);
    return v;
  }

  bool notLhs() {
    int i = 1;
    var c = (_input as CommonTokenStream).LT(i);
    while (c != null && c.type == bnfLexer.TOKEN_WS) {
      c = (_input as CommonTokenStream).LT(++i);
    }
    if (c != null && c.type != bnfLexer.TOKEN_X1) {
      return true;
    }
    // '<'
    while (true) {
      while (c != null && c.type == bnfLexer.TOKEN_WS) {
        c = (_input as CommonTokenStream).LT(++i);
      }
      if (c != null && c.type != bnfLexer.TOKEN_ID && c.type != bnfLexer.TOKEN_X2) {
        return true;
      }
      // ID
      if (c == null) {
        return true;
      }
      if (c.type == bnfLexer.TOKEN_X2) {
        break;
      }
    }
    // '>'
    while (c != null && c.type == bnfLexer.TOKEN_WS) {
      c = (_input as CommonTokenStream).LT(++i);
    }
    if (c?.type == bnfLexer.TOKEN_ASSIGN1) return false;
    if (c?.type == bnfLexer.TOKEN_ASSIGN2) return false;
    if (c?.type == bnfLexer.TOKEN_ASSIGN3) return false;
    if (c?.type == bnfLexer.TOKEN_ASSIGN4) return false;
    return true;
  }
}
