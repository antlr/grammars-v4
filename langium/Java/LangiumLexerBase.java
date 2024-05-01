import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;

public abstract class LangiumLexerBase extends Lexer {

  protected LangiumLexerBase(CharStream input) {
    super(input);
  }

  protected boolean NoSlash() {
    return this._input.LA(1) != '/';
  }
}
