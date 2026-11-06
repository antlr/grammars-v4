package org.antlr.grammars;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;

final class OperatorWhitespaceCheck {
    private static final class ParseResult {
        private final List<String> errors;

        private ParseResult(List<String> errors) {
            this.errors = errors;
        }
    }

    private static final class LexResult {
        private final KotlinLexer lexer;
        private final List<? extends Token> tokens;

        private LexResult(KotlinLexer lexer, List<? extends Token> tokens) {
            this.lexer = lexer;
            this.tokens = tokens;
        }
    }

    public static void main(String[] args) throws IOException {
        assertHasErrors("spaced postfix assertion", parseExpression("value! !").errors);
        assertHasErrors("spaced safe-navigation operator", parseExpression("value? .member").errors);

        ParseResult fixture = parse(Files.readString(Path.of("examples/kotlinFile/operator_whitespace.kt")));
        assertNoErrors("operator whitespace fixture", fixture.errors);

        assertNoErrors("CRLF continuations", parse(
            "fun crlf() {\r\n" +
                "    var value = 0\r\n" +
                "    value =\r\n        1\r\n" +
                "    val negated = !\r\n        false\r\n" +
                "    val dotted = value.\r\n        toString()\r\n" +
                "    val safeDotted = value?.\r\n        toString()\r\n" +
                "}").errors);

        for (String source : List.of(
                 "fun valid(value: Any) { val result = value!! }",
                 "fun valid(value: Any) { val result = (value!!) }",
                 "fun valid(value: Boolean) { val result = !!value }",
                 "fun valid(value: Boolean) { val result = \"${!!value}\" }",
                 "fun valid(value: Any) { val result = \"\"\"${value!!}\"\"\" }",
                 "fun valid(value: Boolean) { val result = ! !value }",
                 "fun valid(value: String?) { val result: String? = value }",
                 "fun valid() { val result = [+\n1] }",
                 "fun valid(value: Boolean) { val result = (!\nvalue) }",
                 "fun valid(value: Boolean) { val result = \"${!\nvalue}\" }",
                 "fun valid(value: Boolean) { val result = \"\"\"${!\nvalue}\"\"\"}")) {
            assertNoErrors("valid operator expression", parse(source).errors);
        }
        assertHasToken("value!!", "EXCL_EXCL");
        assertHasToken("!!value", "EXCL_EXCL");
        assertHasToken("(value!!)", "EXCL_EXCL");
        assertHasToken("\"${!!value}\"", "EXCL_EXCL");
        assertHasToken("\"\"\"${value!!}\"\"\"", "EXCL_EXCL");

        for (String source : List.of(
                 "fun invalid(value: Any) { val result = value! ! }",
                 "fun invalid(value: Any) { val result = value! /* gap */ ! }",
                 "fun invalid(value: Any) { val result = value! // gap\n ! }",
                 "fun invalid(value: Any) { val result = (value!\n!) }",
                 "fun invalid(value: Any) { val result = \"${value! !}\" }",
                 "fun invalid(value: Any) { val result = \"\"\"${value!\n!}\"\"\"}")) {
            assertHasErrors("spaced postfix assertion in " + source, parse(source).errors);
            assertHasToken(source, "EXCL_WS");
        }

        for (String source : List.of(
                 "fun invalid(value: Any) { val result = value? .toString() }",
                 "fun invalid(value: Any) { val result = (value? .toString()) }",
                 "fun invalid(value: Any) { val result = value? /* gap */ .toString() }",
                 "fun invalid(value: Any) { val result = value? // gap\n .toString() }",
                 "fun invalid(value: Any) { val result = \"${value? .toString()}\" }",
                 "fun invalid(value: Any) { val result = \"${value? /* gap */ .toString()}\" }",
                 "fun invalid(value: Any) { val result = \"\"\"${value?\n.toString()}\"\"\"}")) {
            assertHasErrors("spaced safe-navigation operator in " + source, parse(source).errors);
            assertHasToken(source, "SAFE_NAV_WS");
        }

        for (String source : List.of("value?.member", "(value?.member)", "\"${value?.member}\"", "\"\"\"${value?.member}\"\"\"")) {
            assertAdjacentTokens(source, "QUEST", "DOT");
            assertNoToken(source, "SAFE_NAV_WS");
        }
        assertHasToken("value? .member", "SAFE_NAV_WS");
        System.out.println("Operator whitespace checks passed.");
    }

    private static ParseResult parse(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        parser.kotlinFile();
        return new ParseResult(errors);
    }

    private static ParseResult parseExpression(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        parser.expression();
        if (parser.getCurrentToken().getType() != Token.EOF) {
            errors.add("Unconsumed input " + parser.getCurrentToken().getText());
        }
        return new ParseResult(errors);
    }

    private static LexResult lex(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        lexer.removeErrorListeners();
        return new LexResult(lexer, lexer.getAllTokens());
    }

    private static void assertHasToken(String source, String expectedType) {
        LexResult result = lex(source);
        if (result.tokens.stream().noneMatch(token -> expectedType.equals(result.lexer.getVocabulary().getSymbolicName(token.getType())))) {
            throw new AssertionError("Expected " + expectedType + " in " + source + " but found " + describe(result));
        }
    }

    private static void assertNoToken(String source, String unexpectedType) {
        LexResult result = lex(source);
        if (result.tokens.stream().anyMatch(token -> unexpectedType.equals(result.lexer.getVocabulary().getSymbolicName(token.getType())))) {
            throw new AssertionError("Did not expect " + unexpectedType + " in " + source + " but found " + describe(result));
        }
    }

    private static void assertAdjacentTokens(String source, String firstType, String secondType) {
        LexResult result = lex(source);
        for (int i = 0; i + 1 < result.tokens.size(); i++) {
            Token first = result.tokens.get(i);
            Token second = result.tokens.get(i + 1);
            if (firstType.equals(result.lexer.getVocabulary().getSymbolicName(first.getType())) &&
                secondType.equals(result.lexer.getVocabulary().getSymbolicName(second.getType())) &&
                first.getStopIndex() + 1 == second.getStartIndex()) {
                return;
            }
        }
        throw new AssertionError("Expected adjacent " + firstType + " and " + secondType + " in " + source + " but found " + describe(result));
    }

    private static String describe(LexResult result) {
        return result.tokens.stream()
            .map(token -> result.lexer.getVocabulary().getSymbolicName(token.getType()) + " " + token.getText())
            .toList().toString();
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
            throw new AssertionError(label + " should produce a parse error");
        }
    }
}
