package org.antlr.grammars;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTree;

final class RequiredSyntaxCheck {
    private static final class ParseResult {
        private final Parser parser;
        private final ParserRuleContext tree;
        private final List<String> errors;

        private ParseResult(Parser parser, ParserRuleContext tree, List<String> errors) {
            this.parser = parser;
            this.tree = tree;
            this.errors = errors;
        }
    }

    public static void main(String[] args) throws IOException {
        String fixture = Files.readString(Path.of("examples/kotlinFile/required_syntax.kt"));
        ParseResult result = parse(fixture);
        assertNoErrors("required syntax fixture", result.errors);
        assertCount(result, "enumEntry", 6);
        assertCount(result, "catchBlock", 3);
        assertCount(result, "finallyBlock", 2);
        assertCount(result, "annotatedLambda", 10);

        ParseResult crlf = parse(fixture.replace("\n", "\r\n"));
        assertNoErrors("CRLF required syntax fixture", crlf.errors);
        assertCount(crlf, "enumEntry", 6);
        assertCount(crlf, "catchBlock", 3);
        assertCount(crlf, "finallyBlock", 2);
        assertCount(crlf, "annotatedLambda", 10);

        for (String source : List.of(
            "fun test() { try {} catch (problem: Exception) {} }",
            "fun test() { try {} finally {} }",
            "fun test() { try {} catch (first: Exception) {} catch (second: RuntimeException) {} finally {} }",
            "enum class Single { ONE }",
            "enum class Multiple { ONE, TWO, THREE, }",
            "enum class Members { ONE, TWO; fun helper() {} }",
            "enum class Commented { ONE /* comment */, TWO }",
            "fun test() { target {} }",
            "fun test() { target() {} }",
            "fun test() { target<String> {} }",
            "fun test() { target()() {} }",
            "fun test() { target() {}() }",
            "fun test() { target[0] {} }",
            "fun test() { target label@ {} }",
            "fun test() { target @Suppress(\"unused\") {} }",
            "fun test() { target() {}.invoke {} }",
            "fun test() { consume(target {}()) }")) {
            assertNoErrors("valid required syntax source " + source, parse(source).errors);
        }

        for (String source : List.of(
            "fun test() { try {} }",
            "fun test() { try {} finally {} catch (problem: Exception) {} }",
            "enum class MissingComma { FIRST SECOND }",
            "enum class NewlineSeparator { FIRST\nSECOND }",
            "enum class CommentSeparator { FIRST /* no comma */ SECOND }",
            "enum class RepeatedComma { FIRST,, SECOND }",
            "enum class SemicolonSeparator { FIRST; SECOND }",
            "fun test() { target {} {} }",
            "fun test() { target() {} {} }",
            "fun test() { val result = target {} {} }",
            "fun test() { consume(target {} {}) }",
            "fun test() { val result = (target {} {}) }",
            "fun test() { val text = \"${target {} {}}\" }")) {
            assertHasErrors("invalid required syntax source " + source, parse(source).errors);
        }

        System.out.println("Required syntax checks passed.");
    }

    private static void assertCount(ParseResult result, String rule, int expected) {
        int actual = findRules(result.tree, result.parser, rule).size();
        if (actual != expected) {
            throw new AssertionError("Expected " + expected + " " + rule + " nodes but found " + actual);
        }
    }

    private static List<ParserRuleContext> findRules(ParseTree tree, Parser parser, String rule) {
        List<ParserRuleContext> result = new ArrayList<>();
        collectRules(tree, parser, rule, result);
        return result;
    }

    private static void collectRules(ParseTree tree, Parser parser, String rule, List<ParserRuleContext> result) {
        if (tree instanceof ParserRuleContext context && parser.getRuleNames()[context.getRuleIndex()].equals(rule)) {
            result.add(context);
        }
        for (int i = 0; i < tree.getChildCount(); i++) {
            collectRules(tree.getChild(i), parser, rule, result);
        }
    }

    private static ParseResult parse(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        return new ParseResult(parser, parser.kotlinFile(), errors);
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
