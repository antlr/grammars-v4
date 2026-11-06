package org.antlr.grammars;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTree;

final class StatementExpressionCheck {
    private static final class ParseResult {
        private final Parser parser;
        private final ParserRuleContext tree;
        private final List<String> errors;

        private ParseResult(Parser parser, ParserRuleContext tree, List<String> errors) {
            this.parser = parser;
            this.tree = tree;
            this.errors = errors;
        }

        private Set<String> rules() {
            Set<String> result = new HashSet<>();
            collectRules(tree, parser, result);
            return result;
        }
    }

    public static void main(String[] args) throws IOException {
        ParseResult file = parse(Files.readString(Path.of("examples/kotlinFile/statement_expressions.kt")), false);
        assertNoErrors("statement fixture", file.errors);
        assertRules("statement fixture", file.rules(), "declaration", "assignment", "loopStatement", "whenSubject", "labelDefinition");
        assertNoRules("statement fixture", file.rules(), "loopExpression");

        ParseResult script = parse(Files.readString(Path.of("examples/script/statements.kts")), true);
        assertNoErrors("script statement fixture", script.errors);
        assertRules("script statement fixture", script.rules(), "declaration", "assignment", "loopStatement");
        assertNoRules("script statement fixture", script.rules(), "loopExpression");

        for (String expression : List.of(
            "value = 1",
            "value = next = 1",
            "value += 1",
            "for (item in items) item",
            "while (ready) 1",
            "do 1 while (ready)",
            "val name",
            "(value = 1)",
            "(for (item in items) item)",
            "(val name)",
            "consume(for (item in items) item)",
            "consume(val name)",
            "if (value = 1) value",
            "if (for (item in items) item) value",
            "if (val name) value",
            "[value = 1]",
            "[for (item in items) item]",
            "[val name]",
            "\"${for (item in items) item}\"",
            "\"${val name}\"",
            "\"${value = 1}\""
        )) {
            ParseResult result = parse("fun invalid() { val result = " + expression + " }", false);
            assertHasErrors("invalid value expression " + expression, result.errors);
        }

        ParseResult namedArgument = parse("fun valid() { consume(value = 1) }", false);
        assertNoErrors("named argument remains valid", namedArgument.errors);
        System.out.println("Statement/expression checks passed.");
    }

    private static ParseResult parse(String source, boolean script) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        ParserRuleContext tree = script ? parser.script() : parser.kotlinFile();
        return new ParseResult(parser, tree, errors);
    }

    private static void collectRules(ParseTree tree, Parser parser, Set<String> rules) {
        if (tree instanceof ParserRuleContext context) {
            rules.add(parser.getRuleNames()[context.getRuleIndex()]);
        }
        for (int i = 0; i < tree.getChildCount(); i++) {
            collectRules(tree.getChild(i), parser, rules);
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

    private static void assertRules(String label, Set<String> actual, String... expected) {
        for (String rule : expected) {
            if (!actual.contains(rule)) {
                throw new AssertionError(label + " expected parse-tree rule " + rule + " but got " + actual);
            }
        }
    }

    private static void assertNoRules(String label, Set<String> actual, String... unexpected) {
        for (String rule : unexpected) {
            if (actual.contains(rule)) {
                throw new AssertionError(label + " unexpectedly contained parse-tree rule " + rule + ": " + actual);
            }
        }
    }
}
