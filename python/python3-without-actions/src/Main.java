import org.antlr.v4.runtime.*;

public class Main {
    public static void main(String[] args) throws Exception {
        var input = CharStreams.fromFileName(args[0]);
        var lexer = new LexerWithIndentDedentInjector(input);
        var tokens = new CommonTokenStream(lexer);
        var parser = new Python3Parser(tokens);

        System.out.println(input.toString()); // displaying the input source code

        System.out.println();
        System.out.println("Displaying Format:");
        System.out.println("[@TokenIndex,startIndex:stopIndex='text',<TokenType is RuleName>,line:charPositionInLine]");
        System.out.println("-----------------------------------------------------------------------------------------");
        tokens.fill(); // explicit loading all tokens from the lexer until the EOF
        for (Token t : tokens.getTokens()) {
            System.out.println(getTokenMetaDataWithRuleName(parser, t)); // displaying a token metadata
        }

        parser.removeErrorListeners();
        parser.addErrorListener(new IndentationErrorListener());
        for (String errMsg : lexer.getErrorMessages()) { // adding lexer error messages before the parser error messages
            parser.notifyErrorListeners(errMsg);
        }

        parser.file_input(); // begin the parsing at the file_input grammar rule and displaying the lexer and the parser error messages

        if (lexer.getWarnings().size() > 0) {
            System.err.println();
            System.err.println("WARNING:");
            for (String w : lexer.getWarnings()) {
                System.err.println(w); // displaying a warning
            }
        }
    }

    private static String getTokenMetaDataWithRuleName(Python3Parser parser, Token t) {
        final String metaData = t.toString();      // original format: [@TokenIndex,startIndex:stopIndex='text',<TokenType>,line:charPositionInLine]
        final int greaterPos = metaData.lastIndexOf(">,");
        return metaData.substring(0, greaterPos) + // modified format: [@TokenIndex,startIndex:stopIndex='text',<TokenType is RuleName>,line:charPositionInLine]
               " is " + parser.getVocabulary().getDisplayName(t.getType()) +
               metaData.substring(greaterPos);
    }
}