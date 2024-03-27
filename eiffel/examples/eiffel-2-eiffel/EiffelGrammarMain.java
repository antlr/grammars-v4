import static java.lang.System.*;
import java.io.IOException;
import java.io.File;
import java.io.PrintWriter;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

public class EiffelGrammarMain {
   public final static String CodeGenDir = "CODEGEN";

   public static void main(String[] args) {
      try {
         File outPath = new File(CodeGenDir);
         if (!outPath.exists()) {
            outPath.mkdir();
         }
         else if (!outPath.isDirectory()) {
            err.println("ERROR: path \""+CodeGenDir+"\" is not a directory!");
            exit(1);
         }
         for(int i = 0; i < args.length; i++)
         {
            String filename = args[i];
            String outfilename = CodeGenDir+File.separator+new File(filename).getName();
            out.println("Compiling "+filename+" to "+outfilename);
            // create a CharStream that reads from standard input:
            CharStream input = CharStreams.fromFileName(filename);
            // create a lexer that feeds off of input CharStream:
            EiffelGrammarLexer lexer = new EiffelGrammarLexer(input);
            // create a buffer of tokens pulled from the lexer:
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            // create a parser that feeds off the tokens buffer:
            EiffelGrammarParser parser = new EiffelGrammarParser(tokens);
            // replace error listener:
            //parser.removeErrorListeners(); // remove ConsoleErrorListener
            //parser.addErrorListener(new ErrorHandlingListener());
            // begin parsing at class_declaration rule:
            ParseTree tree = parser.class_declaration();
            if (parser.getNumberOfSyntaxErrors() == 0) {
               PrintWriter fout = new PrintWriter(new File(outfilename));
               // print LISP-style tree:
               // out.println(tree.toStringTree(parser));
               CodeGenerator codeGen = new CodeGenerator(tokens);
               codeGen.visit(tree);
               codeGen.print(fout);
               fout.close();
            }
         }
      }
      catch(IOException e) {
         e.printStackTrace();
         exit(1);
      }
      catch(RecognitionException e) {
         e.printStackTrace();
         exit(1);
      }
   }
}
