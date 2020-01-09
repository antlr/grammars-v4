import org.antlr.v4.runtime.*;

public class Main {
    public static void main(String[] args) throws Exception {
        CharStream input = CharStreams.fromFileName(args[0]);
        LexerWithIndentDedentInjector lexer = new LexerWithIndentDedentInjector(input);
        for (Token t: lexer.getAllTokens()) {
            System.out.println(t.toString());
        }
        System.out.println();
        System.out.println(input.toString());

//        CommonTokenStream tokens = new CommonTokenStream(lexer);
//        Python3Parser parser = new Python3Parser(tokens);
//        RuleContext tree = parser.file_input();
//
//        System.out.print(tree.toStringTree(parser));
    }
}
