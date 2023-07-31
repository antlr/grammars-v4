import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.TokenStream;

public abstract class PythonParserBase extends Parser {
    protected PythonParserBase(TokenStream input) {
        super(input);
        //this.removeErrorListeners();
        this.addErrorListener(new IndentationErrorListener());
    }

    @Override
    public void exitRule() {
        super.exitRule();
        if (isMatchedEOF()) {
            IndentationErrorListener.displayErrors();
        }
    }
}
