
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;
import java.util.ArrayDeque;
import java.util.Deque;

public abstract class PostgreSQLLexerBase extends Lexer {
  protected final Deque<String> tags = new ArrayDeque<>();

  protected PostgreSQLLexerBase(CharStream input) {
    super(input);
  }

  public void pushTag() { tags.push(getText()); }

  public boolean isTag() { return getText().equals(tags.peek()); }

  public void popTag() {
    tags.pop();
  }
}