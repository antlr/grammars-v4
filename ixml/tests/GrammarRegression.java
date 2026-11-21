import java.nio.file.Files;
import java.nio.file.Path;
import org.antlr.v4.runtime.*;

/** Count both lexer and parser errors so recovery cannot hide rejection. */
public class GrammarRegression {
    private static int checks;
    private static ixmlParser.IxmlContext check(String source, boolean valid) {
        int[] errors = {0};
        BaseErrorListener listener = new BaseErrorListener() {
            @Override public void syntaxError(Recognizer<?, ?> recognizer, Object offending,
                    int line, int column, String message, RecognitionException exception) {
                errors[0]++;
            }
        };
        ixmlLexer lexer = new ixmlLexer(CharStreams.fromString(source));
        lexer.removeErrorListeners();
        lexer.addErrorListener(listener);
        ixmlParser parser = new ixmlParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(listener);
        ixmlParser.IxmlContext tree = parser.ixml();
        if ((errors[0] == 0) != valid)
            throw new AssertionError("Expected " + (valid ? "acceptance: " : "rejection: ") + source);
        checks++;
        return tree;
    }

    public static void main(String[] args) throws Exception {
        String[] valid = {
            "a.b: 'x'.", "a.b: a.b.", "a.: 'x'.", "a: b..",
            "a..b: 'x'.", "a.1-\u0301: 'x'.", "s: s.",
            "ixml.version: 'x'.", "a: b. c: 'x'.", "a: 'x'.{nested {comment}}b: 'y'.",
            "ixml version '1.0'. s: 'x'.", "ixml{a}version{b}'1.0'. s: 'x'.",
            "s: ['a'-'z']; [#41-#5A].", "s: [\"\"\"\"-\"z\"].", "s: [''''-'z'].",
            "s: [\"\uD83D\uDE00\"-\"\uD83D\uDE01\"].", "s: 'ab'; \"abc\"; ''''; \"\"\"\".",
            "s: .", "s: 'a'; .", "s: ['ab']; ~[].", "s: ('x'), #41.",
            "s: ['a' - 'z']; 'a' ** ',' ."
        };
        for (String source : valid) check(source, true);
        String[] invalid = {
            "s: \"\".", "s: ''.", "s: [\"\"].", "s: +''.",
            "s: [\"ab\"-\"z\"].", "s: ['a'-'yz'].", "s: [''''''-'z'].",
            "ixmlversion\"1\".", "ixmlversion '1.0'. s: 'x'.",
            "ixml version'1.0'. s: 'x'.", "a:'x'.b:'y'.", "a:'x'.@b:'y'.",
            "a: b.\"x\".", ".a: 'x'.", "a. b: 'x'.", "s: # 41.",
            "s: 'a\nb'.", "s: 'a\rb'."
        };
        for (String source : invalid) check(source, false);
        String dotted = check("a.b: a.b.", true).rule_(0).name().getText();
        if (!dotted.equals("a.b")) throw new AssertionError("Wrong dotted name: " + dotted);
        String trailing = check("a.: a..", true).rule_(0).name().getText();
        if (!trailing.equals("a.")) throw new AssertionError("Lost trailing name dot: " + trailing);
        for (String file : args) {
            try { check(Files.readString(Path.of(file)), true); }
            catch (AssertionError e) { throw new AssertionError("Fixture failed: " + file, e); }
        }
        System.out.println("Passed " + checks + " iXML syntax checks.");
    }
}
