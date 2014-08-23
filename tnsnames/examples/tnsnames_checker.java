// Import ANTLR's runtime libraries
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

public class tnsnames_checker {
    public static void main(String[] args) throws Exception {
    
        System.out.println("Tnsnames Checker.");
        
        // Assume tnsnames.ora will be piped via stdin.
        ANTLRInputStream input = new ANTLRInputStream(System.in);

        // The lexer reads from the input CharStream
        tnsnamesLexer lexer = new tnsnamesLexer(input);

        // Fetch a list of lexer tokens. For the parser.
        CommonTokenStream tokens = new CommonTokenStream(lexer);

        // Stuff those tokens into the parser.
        tnsnamesParser parser = new tnsnamesParser(tokens);
        System.out.println("Using grammar defined in " + parser.getGrammarFileName() + "." );

        // And start parsing the tnsnames.ora file on stdin.
        // Parsing is deemed to be from the 'tnsnames' rule.
        System.out.println("Parsing ....");
        ParseTree tree = parser.tnsnames();
        System.out.println("Done.");
        
        // Freak someone out! Print the entire parse tree.
        // System.out.println(tree.toStringTree(parser));
    }
}
