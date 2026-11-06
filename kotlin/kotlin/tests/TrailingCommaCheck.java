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

final class TrailingCommaCheck {
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
        ParseResult fixture = parse(Files.readString(Path.of("examples/kotlinFile/trailing_commas.kt")));
        assertNoErrors("trailing comma fixture", fixture.errors);
        assertCount(fixture, "functionTypeParameters", 3);
        assertCount(fixture, "lambdaParameter", 4);
        assertCount(fixture, "arrayAccess", 2);
        assertCount(fixture, "whenCondition", 4);
        assertCount(fixture, "catchBlock", 1);
        assertCount(fixture, "setter", 1);

        ParseResult crlf = parse(Files.readString(Path.of("examples/kotlinFile/trailing_commas.kt")).replace("\n", "\r\n"));
        assertNoErrors("CRLF trailing comma fixture", crlf.errors);
        assertCount(crlf, "whenCondition", 4);

        for (String source : List.of(
            "fun test() { val lambda = { value, -> value } }",
            "fun test() { val lambda = { first: Int, second, -> first + second } }",
            "fun test() { val lambda = { first: Int,\nsecond,\n-> first + second } }",
            "fun test() { val lambda = { (first, second), -> first + second } }",
            "typealias Empty = () -> Unit",
            "typealias Named = (value: String,) -> Unit",
            "typealias Unnamed = (String, Int,) -> Unit",
            "fun test(values: List<Int>) { values[0,] }",
            "fun test(values: List<Int>) { values[0, 1,] }",
            "fun test(value: Any) { when (value) { 1, 2, -> Unit } }",
            "fun test(value: Any) { when (value) { is String, !is Int, -> Unit } }",
            "fun test() { try { Unit } catch (@Suppress(\"unused\") problem: Exception,) { Unit } }",
            "var value: Int = 0\nset(@Suppress(\"unused\") value: Int,) { field = value }",
            "typealias Continued = (\nString,\nInt,\n) -> Unit",
            "fun test(values: List<Int>) { values[0, /* trailing */] }",
            "fun test(value: Any) { when (value) { 1, /* trailing */ -> Unit } }",
            "fun test() { try { Unit } catch (problem: Exception, /* trailing */) { Unit } }",
            "var value: Int = 0\nset(@Suppress(\"unused\") value: Int, /* trailing */) { field = value }")) {
            assertNoErrors("valid trailing comma source " + source, parse(source).errors);
        }

        for (String source : List.of(
            "fun test() { val lambda = { ,value -> value } }",
            "fun test() { val lambda = { , -> Unit } }",
            "fun test() { val lambda = { value,, other -> value } }",
            "typealias MissingFirst = (, String) -> Unit",
            "typealias Repeated = (String,, Int) -> Unit",
            "fun test(values: List<Int>) { values[] }",
            "fun test(values: List<Int>) { values[ /* empty */ ] }",
            "fun test(value: Any) { when (value) { ,1 -> Unit } }",
            "fun test(value: Any) { when (value) { 1,, 2 -> Unit } }",
            "fun test() { try { Unit } catch (problem: Exception, other: Exception,) { Unit } }",
            "var value: Int = 0\nset(first: Int, second: Int,) { field = first }")) {
            assertHasErrors("invalid list source " + source, parse(source).errors);
        }

        System.out.println("Trailing comma checks passed.");
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
