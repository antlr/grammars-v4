import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.TokenStream;

public abstract class Scala3ParserBase extends Parser {

    protected Scala3ParserBase(TokenStream input) {
        super(input);
    }

    /**
     * Returns true when the --3.0-migration command-line flag is set,
     * enabling Scala 2-compatible syntax (._ wildcard imports, [_] type wildcards).
     */
    protected boolean migration30() {
        return Scala3LexerBase.migration30;
    }
}
