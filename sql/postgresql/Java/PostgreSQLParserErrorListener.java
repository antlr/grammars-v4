import org.antlr.v4.runtime.*;

public class PostgreSQLParserErrorListener extends BaseErrorListener {
    PostgreSQLParser grammar;

    public PostgreSQLParserErrorListener() {
    }

    @Override
    public void syntaxError(final Recognizer<?, ?> recognizer, final Object offendingSymbol, final int line,
                            final int charPositionInLine, final String msg, final RecognitionException e) {
        grammar.ParseErrors.add(new PostgreSQLParseError(0, 0, line, charPositionInLine, msg));
    }

}
