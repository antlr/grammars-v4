import java.util.List;
import java.util.stream.Collectors;
import org.antlr.v4.runtime.*;
import org.junit.Test;
import static org.junit.Assert.*;

public class RexReviewTest {
    private static class Errors extends BaseErrorListener {
        int count;
        @Override
        public void syntaxError(Recognizer<?, ?> recognizer, Object symbol,
                int line, int column, String message, RecognitionException error) {
            count++;
        }
    }

    private rexParser.Grammar_Context parse(String input, boolean valid) {
        Errors errors = new Errors();
        rexLexer lexer = new rexLexer(CharStreams.fromString(input));
        lexer.removeErrorListeners();
        lexer.addErrorListener(errors);
        rexParser parser = new rexParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errors);
        rexParser.Grammar_Context tree = parser.grammar_();
        assertEquals(input, valid, errors.count == 0);
        return tree;
    }

    @Test
    public void commentsAreSkippedWithoutLeadingWhitespace() {
        parse("// first\n/* ordinary */Start ::= 'x'// final", true);
        parse("Start ::= 'x' /* not closed", false);
    }

    @Test
    public void optionsAreNodesNotComments() {
        for (String option : new String[] {
                "/*ws:explicit*/", "/* ws: explicit */", "/*\nws :\n definition\n*/"}) {
            rexParser.Grammar_Context tree = parse("Start ::= 'x' " + option, true);
            assertEquals(1, tree.syntaxDefinition().syntaxProduction(0).option().size());
        }
        parse("Start ::= 'x' /* ws: invalid */", false);
        parse("Start ::= 'x' /* ws: explicit", false);
    }

    @Test
    public void unicodeClassesKeepCodePointBoundaries() {
        rexLexer lexer = new rexLexer(CharStreams.fromString("[#x0041#x0061-#x007A]"));
        List<Integer> types = lexer.getAllTokens().stream()
            .map(Token::getType).collect(Collectors.toList());
        assertEquals(java.util.Arrays.asList(rexLexer.OpenSet, rexLexer.SetUnicode,
            rexLexer.SetUnicodeRange, rexLexer.CloseSet), types);
        parse("Start ::= A <?TOKENS?> A ::= [#x0041-#x005A]+ "
            + "[#x0041] == [#x0061] [#x0041-#x005A] == [#x0061-#x007A]", true);
    }

    @Test
    public void delimiterIsTwoLiteralBackslashes() {
        parse("Start ::= A <?TOKENS?> A ::= 'a' A \\\\ ' '", true);
        parse("Start ::= A <?TOKENS?> A ::= 'a' A \\ ' '", false);
    }
}
