import 'package:antlr4/antlr4.dart';
import 'awkLexer.dart';

abstract class awkLexerBase extends Lexer {
  bool _afterExpr = false;

  awkLexerBase(CharStream input) : super(input);

  @override
  Token nextToken() {
    final token = super.nextToken();
    if (token.channel == Token.DEFAULT_CHANNEL) {
      _afterExpr = token.type == awkLexer.TOKEN_WORD
          || token.type == awkLexer.TOKEN_NUMBER
          || token.type == awkLexer.TOKEN_STRING
          || token.type == awkLexer.TOKEN_BUILTIN_FUNC_NAME
          || token.type == awkLexer.TOKEN_INCR
          || token.type == awkLexer.TOKEN_DECR
          || token.type == awkLexer.TOKEN_Rp
          || token.type == awkLexer.TOKEN_Rb;
    }
    return token;
  }

  bool IsNotAfterExpr() => !_afterExpr;
}
