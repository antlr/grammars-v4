import java.io.PrintWriter;
import org.antlr.v4.runtime.*;

@SuppressWarnings("CheckReturnValue")
public class CodeGenerator extends EiffelGrammarBaseVisitor<String> {

   public CodeGenerator(TokenStream tokens) {
      rewriter = new TokenStreamRewriter(tokens);
   }

   public void print(PrintWriter fout) {
      fout.print(rewriter.getText());
   }

   @Override public String visitClass_declaration(EiffelGrammarParser.Class_declarationContext ctx) {
      rewriter.insertBefore(ctx.start, "-- Automatically cloned Eiffel code!\n\n");
      //rewriter.insertAfter(ctx.stop, "-- That's all folks!\n");
      return visitChildren(ctx);
   }

   private TokenStreamRewriter rewriter;
}
