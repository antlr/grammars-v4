import org.antlr.v4.runtime.*;
import java.util.ArrayList;
import java.util.List;

public abstract class AdaParserBase extends Parser {

    protected AdaParserBase(TokenStream input) {
        super(input);
    }

    protected void ParsePragmas() {
        BufferedTokenStream stream = (BufferedTokenStream) getTokenStream();
        stream.fill();
        List<Token> allTokens = stream.getTokens();
        final int PRAGMA_CHANNEL = 2;
        List<Token> currentPragma = null;
        List<List<Token>> pragmas = new ArrayList<>();
        for (Token token : allTokens) {
            if (token.getChannel() != PRAGMA_CHANNEL)
                continue;
            if (token.getType() == AdaLexer.PRAGMA) {
                currentPragma = new ArrayList<>();
                currentPragma.add(token);
            } else if (currentPragma != null) {
                currentPragma.add(token);
                if (token.getType() == AdaLexer.SEMI) {
                    pragmas.add(currentPragma);
                    currentPragma = null;
                }
            }
        }
        for (List<Token> pragmaTokens : pragmas) {
            List<Token> defaultChannelTokens = new ArrayList<>();
            for (Token t : pragmaTokens) {
                CommonToken ct = new CommonToken(t);
                ct.setChannel(Token.DEFAULT_CHANNEL);
                defaultChannelTokens.add(ct);
            }
            CommonToken eof = new CommonToken(Token.EOF);
            eof.setChannel(Token.DEFAULT_CHANNEL);
            defaultChannelTokens.add(eof);
            ListTokenSource tokenSource = new ListTokenSource(defaultChannelTokens);
            CommonTokenStream tokenStream = new CommonTokenStream(tokenSource);
            AdaParser parser = new AdaParser(tokenStream);
            parser.removeErrorListeners();
            for (ANTLRErrorListener listener : this.getErrorListeners()) {
                parser.addErrorListener(listener);
            }
            parser.pragmaRule();
        }
    }
}
