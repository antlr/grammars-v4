import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;

public abstract class BisonLexerBase extends Lexer {

    int percent_percent_count;

    protected BisonLexerBase(CharStream input) {
        super(input);
    }

    public void NextMode()
    {
        ++percent_percent_count;
        if (percent_percent_count == 1) {
            return;
        } else if (percent_percent_count == 2) {
            this.pushMode(BisonLexer.EpilogueMode);
            return;
        } else {
            this.setType(BisonLexer.PercentPercent);
            return;
        }
    }

    @Override
    public void reset() {
        percent_percent_count = 0;
        super.reset();
    }   
}
