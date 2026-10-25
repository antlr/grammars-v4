import org.antlr.v4.runtime.*;

public abstract class RustParserBase extends Parser {
    public RustParserBase(TokenStream input){
        super(input);
    }

    public boolean NextGT() {
        return _input.LA(1) == RustParser.GT;
    }

    public boolean NextLT() {
        return _input.LA(1) == RustParser.LT;
    }
}