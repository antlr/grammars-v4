import org.antlr.v4.runtime.*;

public abstract class RexxParserBase extends Parser
{
   public RexxParserBase(TokenStream input)
   {
      super(input);
   }

   public final boolean is_prev_token_whitespace() {
      int tokenIndex = getCurrentToken().getTokenIndex();
      int prevTokenType = getTokenStream().get(tokenIndex-1).getType();
      return (prevTokenType == RexxLexer.WHITESPACES);
   }

   public final boolean is_prev_token_not_whitespace() {
      return !is_prev_token_whitespace();
   }

}
