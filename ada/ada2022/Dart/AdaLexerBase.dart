import 'package:antlr4/antlr4.dart';
import 'AdaLexer.dart';

abstract class AdaLexerBase extends Lexer {
    int _lastTokenType = 0;

    AdaLexerBase(CharStream input) : super(input);

    @override
    Token nextToken() {
        var token = super.nextToken();
        if (token.channel == Token.DEFAULT_CHANNEL) {
            _lastTokenType = token.type!;
        }
        return token;
    }

    bool IsCharLiteralAllowed() {
        // In Ada, a tick after an identifier, closing paren, or 'all' keyword
        // is an attribute tick, not the start of a character literal.
        return _lastTokenType != AdaLexer.TOKEN_IDENTIFIER_
            && _lastTokenType != AdaLexer.TOKEN_RP
            && _lastTokenType != AdaLexer.TOKEN_ALL;
    }
}
