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
        assertRules("script statement fixture", script.rules(), "statements", "declaration", "assignment", "loopStatement");
        ParserRuleContext scriptStatements = directRuleChild(script.tree, script.parser, "statements");
        if (scriptStatements == null) {
            throw new AssertionError("script statement fixture expected a direct statements child");
        }
        assertDirectRuleChildrenCount("script statement fixture", scriptStatements, script.parser, "statement", 12);
        assertNoRules("script statement fixture", script.rules(), "loopExpression");

        for (String source : List.of(
            "",
            "val finalDeclaration = 1",
            "package sample\nimport kotlin.math.*\nval first = 1; val second = first + 1\nsecond",
            "fun scriptFunction() {}\nclass ScriptClass\nobject ScriptObject\nvar mutable = 1\n" +
                "mutable = 2\nfor (item in items) println(item)\nwhile (ready) mutable += 1\n" +
                "do mutable += 1 while (ready)\nprintln(mutable)"
        )) {
            ParseResult result = parse(source, true);
            assertNoErrors("script source " + source, result.errors);
            ParserRuleContext statements = directRuleChild(result.tree, result.parser, "statements");
            if (statements == null) {
                throw new AssertionError("script source " + source + " expected a direct statements child");
            }
            if (source.isEmpty()) {
                assertDirectRuleChildrenCount("empty script", statements, result.parser, "statement", 0);
            } else {
                assertDirectRuleChild("script source " + source, statements, result.parser, "statement");
            }
        }

        ParseResult whenSubjects = parse(
            "fun subjects(value: Int) { " +
                "val plain = when (val subject = value) { else -> subject }; " +
                "val typed = when (@Subject val subject: Int = value) { else -> subject }; " +
                "val ordinary = when (value) { else -> 0 }; " +
                "val subjectless = when { else -> 0 } " +
            "}", false);
        assertNoErrors("when subject variables", whenSubjects.errors);
        List<ParserRuleContext> subjectVariables = findRules(whenSubjects.tree, whenSubjects.parser, "whenSubjectVariable");
        if (subjectVariables.size() != 2) {
            throw new AssertionError("Expected two when subject variables but found " + subjectVariables.size());
        }
        for (ParserRuleContext subjectVariable : subjectVariables) {
            if (!(subjectVariable.getParent() instanceof ParserRuleContext parent)
                || !ruleName(parent, whenSubjects.parser).equals("whenSubject")) {
                throw new AssertionError("whenSubjectVariable must be a direct child of whenSubject: " + subjectVariable.getText());
            }
        }
        if (!subjectVariables.get(0).getText().equals("valsubject=value")
            || !subjectVariables.get(1).getText().equals("@Subjectvalsubject:Int=value")) {
            throw new AssertionError("Unexpected when subject variable forms: " + subjectVariables);
        }

        for (String subject : List.of("var subject = value", "val subject", "val (first, second) = value")) {
            ParseResult result = parse("fun invalid() { val result = when (" + subject + ") { else -> 0 } }", false);
            assertHasErrors("invalid when subject " + subject, result.errors);
        }

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

    private static List<ParserRuleContext> findRules(ParseTree tree, Parser parser, String rule) {
        List<ParserRuleContext> result = new ArrayList<>();
        collectRules(tree, parser, rule, result);
        return result;
    }

    private static void collectRules(ParseTree tree, Parser parser, String rule, List<ParserRuleContext> result) {
        if (tree instanceof ParserRuleContext context && ruleName(context, parser).equals(rule)) {
            result.add(context);
        }
        for (int i = 0; i < tree.getChildCount(); i++) {
            collectRules(tree.getChild(i), parser, rule, result);
        }
    }

    private static String ruleName(ParserRuleContext context, Parser parser) {
        return parser.getRuleNames()[context.getRuleIndex()];
    }

    private static void assertDirectRuleChild(String label, ParserRuleContext parent, Parser parser, String childRule) {
        if (directRuleChild(parent, parser, childRule) == null) {
            throw new AssertionError(label + " expected direct child rule " + childRule + " but found " + parent.getText());
        }
    }

    private static ParserRuleContext directRuleChild(ParserRuleContext parent, Parser parser, String childRule) {
        for (int i = 0; i < parent.getChildCount(); i++) {
            if (parent.getChild(i) instanceof ParserRuleContext child && ruleName(child, parser).equals(childRule)) {
                return child;
            }
        }
        return null;
    }

    private static void assertDirectRuleChildrenCount(String label, ParserRuleContext parent, Parser parser, String childRule,
        int expected) {
        int actual = 0;
        for (int i = 0; i < parent.getChildCount(); i++) {
            if (parent.getChild(i) instanceof ParserRuleContext child && ruleName(child, parser).equals(childRule)) {
                actual++;
            }
        }
        if (actual != expected) {
            throw new AssertionError(label + " expected " + expected + " direct " + childRule + " children but found " + actual);
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
