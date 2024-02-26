import org.antlr.v4.runtime.*;

public abstract class PythonParserBase extends Parser {
    protected PythonParserBase(TokenStream input) {
        super(input);
    }

    // https://docs.python.org/3/reference/lexical_analysis.html#soft-keywords
    public boolean isEqualToCurrentTokenText(String tokenText) {
        return this.getCurrentToken().getText().equals(tokenText);
    }

    public boolean isnotEqualToCurrentTokenText(String tokenText) {
        return !this.isEqualToCurrentTokenText(tokenText); // for compatibility with the Python 'not' logical operator
    }
}
