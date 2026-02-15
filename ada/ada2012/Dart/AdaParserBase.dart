import 'package:antlr4/antlr4.dart';
import 'AdaLexer.dart';
import 'AdaParser.dart';

abstract class AdaParserBase extends Parser {
    AdaParserBase(TokenStream input) : super(input);

    void ParsePragmas() {
        var stream = tokenStream as BufferedTokenStream;
        stream.fill();
        var allTokens = stream.getTokens() ?? [];
        const int PRAGMA_CHANNEL = 2;
        List<Token>? currentPragma;
        var pragmas = <List<Token>>[];
        for (var token in allTokens) {
            if (token.channel != PRAGMA_CHANNEL) continue;
            if (token.type == AdaLexer.TOKEN_PRAGMA) {
                currentPragma = [token];
            } else if (currentPragma != null) {
                currentPragma.add(token);
                if (token.type == AdaLexer.TOKEN_SEMI) {
                    pragmas.add(currentPragma);
                    currentPragma = null;
                }
            }
        }
        for (var pragmaTokens in pragmas) {
            var defaultChannelTokens = <Token>[];
            for (var t in pragmaTokens) {
                var ct = CommonToken.copy(t);
                ct.channel = Token.DEFAULT_CHANNEL;
                defaultChannelTokens.add(ct);
            }
            var eof = CommonToken(Token.EOF);
            eof.channel = Token.DEFAULT_CHANNEL;
            defaultChannelTokens.add(eof);
            var tokenSource = ListTokenSource(defaultChannelTokens);
            var tokenStream = CommonTokenStream(tokenSource);
            var parser = AdaParser(tokenStream);
            parser.removeErrorListeners();
            for (var listener in errorListeners) {
                parser.addErrorListener(listener);
            }
            parser.pragmaRule();
        }
    }
}
