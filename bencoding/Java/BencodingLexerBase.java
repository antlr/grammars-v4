import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;

public abstract class BencodingLexerBase extends Lexer {

  private int stringLength = 0;

  protected BencodingLexerBase(CharStream input) {
    super(input);
  }

  protected void setStringLength() {
    String tokenText = super.getText();
    this.stringLength = Integer.parseInt(tokenText.substring(0, tokenText.length() - 1));
  }

  protected boolean consumeStringChars() {
    this.stringLength--;
    return this.stringLength > -1;
  }
}
