import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.Lexer;

public abstract class LuaLexerBase extends Lexer {

    private int start_line;
    private int start_col;

    protected LuaLexerBase(CharStream input) {
        super(input);
    }

    protected void HandleComment()
    {
        start_line = this.getLine();
        start_col = this.getCharPositionInLine() - 2;
        var cs = (CharStream)_input;
        if (cs.LA(1) == '[')
        {
            int sep = skip_sep(cs);
            if (sep >= 2)
            {
                read_long_string(cs, sep);
                return;
            }
        }
        while (cs.LA(1) != '\n' && cs.LA(1) != -1)
        {
            cs.consume();
        }
    }

    private void read_long_string(CharStream cs, int sep)
    {
        boolean done = false;
        cs.consume();
        for (; ; )
        {
            var c = cs.LA(1);
            var cc = (char)c;
            switch (c)
            {
                case -1:
                    done = true;
                    ANTLRErrorListener listener = this.getErrorListenerDispatch();
                    listener.syntaxError(this, null, this.start_line, this.start_col, "unfinished long comment", null);
                    break;
                case ']':
                    if (skip_sep(cs) == sep)
                    {
                        cs.consume();
                        done = true;
                    }
                    break;
                default:
                    if (cs.LA(1) == -1)
                    {
                        done = true;
                        break;
                    }
                    cs.consume();
                    break;
            }
            if (done) break;
        }
    }

    private int skip_sep(CharStream cs)
    {
        int count = 0;
        int s = cs.LA(1);
        char ss = (char)s;
        cs.consume();
        while (cs.LA(1) == '=')
        {
            cs.consume();
            count++;
        }
        if (cs.LA(1) == s) count += 2;
        else if (count == 0) count = 1;
        else count = 0;
        return count;
    }

    public boolean IsLine1Col0()
    {
        CharStream cs = (CharStream)_input;
        if (cs.index() == 1) return true;
        return false;
    }
}
