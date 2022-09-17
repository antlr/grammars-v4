import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;

import java.util.ArrayDeque;
import java.util.Deque;

public abstract class PostgreSQLLexerBase extends Lexer {
    protected final Deque<String> tags = new ArrayDeque<>();

    protected PostgreSQLLexerBase(CharStream input) {
        super(input);

    }

    public void pushTag() {
        tags.push(getText());
    }

    public boolean isTag() {
        return getText().equals(tags.peek());
    }

    public void popTag() {
        tags.pop();
    }

    public boolean checkLA(int c) {
        return getInputStream().LA(1) != c;
    }

    public boolean charIsLetter() {
        return Character.isLetter(getInputStream().LA(-1));
    }

    public void HandleNumericFail() {
        getInputStream().seek(getInputStream().index() - 2);
        setType(PostgreSQLLexer.Integral);
    }

    public void HandleLessLessGreaterGreater() {
        if (getText() == "<<") setType(PostgreSQLLexer.LESS_LESS);
        if (getText() == ">>") setType(PostgreSQLLexer.GREATER_GREATER);
    }

    public void UnterminatedBlockCommentDebugAssert() {
        //Debug.Assert(InputStream.LA(1) == -1 /*EOF*/);
    }

    public boolean CheckIfUtf32Letter() {
        int codePoint = getInputStream().LA(-2) << 8 + getInputStream().LA(-1);
        char[] c;
        if (codePoint < 0x10000) {
            c = new char[]{(char) codePoint};
        } else {
            codePoint -= 0x10000;
            c = new char[]{(char) (codePoint / 0x400 + 0xd800), (char) (codePoint % 0x400 + 0xdc00)};
        }
        return Character.isLetter(c[0]);
    }
}