package org.antlr.grammars;

import java.util.ArrayList;
import java.util.List;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;

final class LineEndingCheck {
    private static final class LexResult {
        private final KotlinLexer lexer;
        private final List<? extends Token> tokens;
        private final List<String> errors;

        private LexResult(KotlinLexer lexer, List<? extends Token> tokens, List<String> errors) {
            this.lexer = lexer;
            this.tokens = tokens;
            this.errors = errors;
        }
    }

    public static void main(String[] args) {
        assertNewlineTokens();
        assertParsesWithLineEndings();
        assertSuppressedLineEndings();
        assertOperatorTrivia();
        assertShebangPosition();
        System.out.println("Line ending checks passed.");
    }

    private static void assertNewlineTokens() {
        LexResult result = lex("\r\n\n\r");
        assertNoErrors("CR, LF, and CRLF newline tokens", result.errors);
        List<String> newlines = new ArrayList<>();
        for (Token token : result.tokens) {
            if ("NL".equals(symbolicName(result, token))) {
                newlines.add(token.getText());
            }
        }
        if (!newlines.equals(List.of("\r\n", "\n", "\r"))) {
            throw new AssertionError("Expected one NL token for each line ending but found " + printable(newlines));
        }
    }

    private static void assertParsesWithLineEndings() {
        for (String lineEnding : List.of("\r", "\n", "\r\n")) {
            String file = "package sample" + lineEnding
                + "// line comment" + lineEnding
                + "fun test() {" + lineEnding
                + "    val first = 1" + lineEnding
                + "    if (true) {" + lineEnding
                + "        val nested = first" + lineEnding
                + "    }" + lineEnding
                + "    val last = first" + lineEnding
                + "}" + lineEnding;
            assertNoErrors("Kotlin file with " + printable(lineEnding), parse(file, false));

            String script = "val first = 1" + lineEnding + "val second = first" + lineEnding;
            assertNoErrors("Kotlin script with " + printable(lineEnding), parse(script, true));
        }
    }

    private static void assertSuppressedLineEndings() {
        String source = "val parenthesized = (1\r+ 2);"
            + "val indexed = values[0\r+ 1];"
            + "val template = \"\"\"${1\r+ 2}\"\"\"";
        ParseResult result = parse(source, false);
        assertNoErrors("suppressed CR in parentheses, brackets, and template expressions", result.errors);
        LexResult tokens = lex(source);
        assertNoErrors("suppressed CR tokenization", tokens.errors);
        for (Token token : tokens.tokens) {
            if ("NL".equals(symbolicName(tokens, token))) {
                throw new AssertionError("Expected CR newlines to be suppressed in nested modes but found " + describe(tokens));
            }
        }
    }

    private static void assertOperatorTrivia() {
        LexResult result = lex("value !is\rString; value !in\rvalues; value !\r!other; value ?\r.other");
        assertNoErrors("CR operator trivia", result.errors);
        assertContainsTokenText(result, "NOT_IS", "!is\r");
        assertContainsTokenText(result, "NOT_IN", "!in\r");
        assertContainsTokenText(result, "EXCL_WS", "!\r");
        assertContainsTokenText(result, "SAFE_NAV_WS", "?\r.");

        assertNoErrors("CR operator boundaries in expressions", parse(
            "fun test(value: Any, values: List<Int>) { val type = value !is\rString; val member = 1 !in\rvalues }", false));
        assertHasErrors("trivia-separated non-null assertion", parse("fun test(value: Any) { value !\r! }", false));
        assertHasErrors("trivia-separated safe navigation", parse("fun test(value: Any) { value ?\r.member }", false));
    }

    private static void assertShebangPosition() {
        for (String lineEnding : List.of("\r", "\n", "\r\n")) {
            String source = "#!/usr/bin/env kotlin" + lineEnding + "fun test() {}";
            LexResult result = lex(source);
            assertNoErrors("initial shebang with " + printable(lineEnding), result.errors);
            assertInitialShebang(result, "#!/usr/bin/env kotlin");
            assertNoErrors("parse initial shebang with " + printable(lineEnding), parse(source, false));
            assertNoErrors("parse script initial shebang with " + printable(lineEnding), parse(source, true));
        }

        LexResult atEof = lex("#!");
        assertNoErrors("initial shebang at EOF", atEof.errors);
        assertInitialShebang(atEof, "#!");
        assertNoErrors("parse initial shebang at EOF", parse("#!", false));
        assertNoErrors("parse script initial shebang at EOF", parse("#!", true));

        for (String source : List.of(
            " \n#!/usr/bin/env kotlin\n",
            " \r#!/usr/bin/env kotlin\r",
            "\t\r\n#!/usr/bin/env kotlin\r\n",
            "// preceding comment\r#!/usr/bin/env kotlin\r",
            "/* preceding comment */ #!/usr/bin/env kotlin\n")) {
            LexResult result = lex(source);
            assertNoErrors("lex noninitial shebang spelling " + printable(source), result.errors);
            assertNoToken(result, "ShebangLine");
            assertContainsToken(result, "InvalidShebangLine");
            assertHasErrors("parse noninitial shebang spelling " + printable(source), parse(source, false));
            assertHasErrors("parse script noninitial shebang spelling " + printable(source), parse(source, true));
        }

        for (String source : List.of("package sample\n#!/usr/bin/env kotlin\n", "fun test() { #!/usr/bin/env kotlin }")) {
            LexResult result = lex(source);
            assertNoErrors("lex later shebang spelling " + printable(source), result.errors);
            assertNoToken(result, "ShebangLine");
            assertContainsToken(result, "InvalidShebangLine");
            assertHasErrors("parse later shebang spelling " + printable(source), parse(source, false));
            assertHasErrors("parse script later shebang spelling " + printable(source), parse(source, true));
        }
    }

    private static void assertInitialShebang(LexResult result, String expectedText) {
        for (Token token : result.tokens) {
            if ("ShebangLine".equals(symbolicName(result, token))) {
                if (token.getStartIndex() != 0 || token.getChannel() != Token.DEFAULT_CHANNEL || !expectedText.equals(token.getText())) {
                    throw new AssertionError("Initial shebang has unexpected position, channel, or text: " + describe(result));
                }
                return;
            }
        }
        throw new AssertionError("Expected an initial ShebangLine but found " + describe(result));
    }

    private static void assertContainsTokenText(LexResult result, String type, String expectedText) {
        for (Token token : result.tokens) {
            if (type.equals(symbolicName(result, token)) && expectedText.equals(token.getText())) {
                return;
            }
        }
        throw new AssertionError("Expected " + type + " with text " + printable(expectedText) + " but found " + describe(result));
    }

    private static void assertContainsToken(LexResult result, String type) {
        for (Token token : result.tokens) {
            if (type.equals(symbolicName(result, token))) {
                return;
            }
        }
        throw new AssertionError("Expected " + type + " but found " + describe(result));
    }

    private static void assertNoToken(LexResult result, String type) {
        for (Token token : result.tokens) {
            if (type.equals(symbolicName(result, token))) {
                throw new AssertionError("Did not expect " + type + " but found " + describe(result));
            }
        }
    }

    private static ParseResult parse(String source, boolean script) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        if (script) {
            parser.script();
        } else {
            parser.kotlinFile();
        }
        return new ParseResult(errors);
    }

    private static LexResult lex(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        return new LexResult(lexer, lexer.getAllTokens(), errors);
    }

    private static String symbolicName(LexResult result, Token token) {
        return result.lexer.getVocabulary().getSymbolicName(token.getType());
    }

    private static String describe(LexResult result) {
        return result.tokens.stream()
            .map(token -> symbolicName(result, token) + " " + printable(token.getText()))
            .toList().toString();
    }

    private static String printable(String value) {
        return value.replace("\r", "\\r").replace("\n", "\\n");
    }

    private static String printable(List<String> values) {
        return values.stream().map(LineEndingCheck::printable).toList().toString();
    }

    private static BaseErrorListener errorCollector(List<String> errors) {
        return new BaseErrorListener() {
            @Override
            public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine,
                String message, RecognitionException exception) {
                errors.add(line + ":" + charPositionInLine + " " + message);
            }
        };
    }

    private static void assertNoErrors(String label, List<String> errors) {
        if (!errors.isEmpty()) {
            throw new AssertionError(label + " produced errors: " + errors);
        }
    }

    private static void assertHasErrors(String label, List<String> errors) {
        if (errors.isEmpty()) {
            throw new AssertionError(label + " should produce a parser error");
        }
    }

    private static void assertNoErrors(String label, ParseResult result) {
        assertNoErrors(label, result.errors);
    }

    private static void assertHasErrors(String label, ParseResult result) {
        assertHasErrors(label, result.errors);
    }

    private static final class ParseResult {
        private final List<String> errors;

        private ParseResult(List<String> errors) {
            this.errors = errors;
        }
    }
}
