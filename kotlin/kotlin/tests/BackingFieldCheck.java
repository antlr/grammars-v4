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

final class BackingFieldCheck {
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
        String fixture = Files.readString(Path.of("examples/kotlinFile/backing_fields.kt"));
        ParseResult result = parse(fixture);
        assertNoErrors("backing field fixture", result.errors);
        assertBackingFields(result, List.of(
            new FieldShape(false, true),
            new FieldShape(true, false),
            new FieldShape(true, true),
            new FieldShape(false, true),
            new FieldShape(true, false)));
        assertCount(result, "assignment", 1);

        ParseResult crlf = parse(fixture.replace("\n", "\r\n"));
        assertNoErrors("CRLF backing field fixture", crlf.errors);
        assertBackingFields(crlf, List.of(
            new FieldShape(false, true),
            new FieldShape(true, false),
            new FieldShape(true, true),
            new FieldShape(false, true),
            new FieldShape(true, false)));

        for (String source : List.of(
            "val inferred\n    field = buildValue()",
            "val typed: String\n    field: String",
            "val typedInitialized: List<String>\n    field: List<String> = listOf(\"value\")",
            "val nullable: String?\n    field: String?",
            "val function: () -> String\n    field: () -> String = { \"value\" }",
            "val multiline: Int\n    field =\n        buildValue()",
            "class Compact { val inferred\nfield = 1; val typed: Int\nfield: Int }",
            "var value: Int = 0\n    set(value) { field = value }")) {
            ParseResult valid = parse(source);
            assertNoErrors("valid backing field source " + source, valid.errors);
        }

        for (String source : List.of(
            "val bare: Int\n    field",
            "val missingFieldType: Int\n    field:",
            "val missingFieldInitializer: Int\n    field =")) {
            assertHasErrors("invalid backing field source " + source, parse(source).errors);
        }

        System.out.println("Backing field checks passed.");
    }

    private static void assertBackingFields(ParseResult result, List<FieldShape> expected) {
        List<KotlinParser.ExplicitBackingFieldContext> fields = new ArrayList<>();
        collectBackingFields(result.tree, fields);
        if (fields.size() != expected.size()) {
            throw new AssertionError("Expected " + expected.size() + " explicit backing fields but found " + fields.size());
        }
        for (int i = 0; i < fields.size(); i++) {
            KotlinParser.ExplicitBackingFieldContext field = fields.get(i);
            FieldShape shape = expected.get(i);
            if (!(field.getParent() instanceof KotlinParser.PropertyDeclarationContext)) {
                throw new AssertionError("Explicit backing field is not a direct property child: " + field.getText());
            }
            if ((field.type() != null) != shape.hasType || (field.expression() != null) != shape.hasExpression) {
                throw new AssertionError("Unexpected explicit backing field shape: " + field.getText());
            }
        }
    }

    private static void collectBackingFields(ParseTree tree, List<KotlinParser.ExplicitBackingFieldContext> result) {
        if (tree instanceof KotlinParser.ExplicitBackingFieldContext field) {
            result.add(field);
        }
        for (int i = 0; i < tree.getChildCount(); i++) {
            collectBackingFields(tree.getChild(i), result);
        }
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

    private static final class FieldShape {
        private final boolean hasType;
        private final boolean hasExpression;

        private FieldShape(boolean hasType, boolean hasExpression) {
            this.hasType = hasType;
            this.hasExpression = hasExpression;
        }
    }
}
