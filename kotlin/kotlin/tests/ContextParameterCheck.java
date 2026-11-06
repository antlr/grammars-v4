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

final class ContextParameterCheck {
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
        String fixture = Files.readString(Path.of("examples/kotlinFile/context_parameters.kt"));
        ParseResult parsedFixture = parse(fixture);
        assertNoErrors("context parameter fixture", parsedFixture.errors);
        assertModifierOwnership(parsedFixture, 9, 5, 4);
        assertCount(parsedFixture, "contextParameter", 10);
        assertAnnotatedContextParameters(parsedFixture, 2);

        ParseResult crlf = parse(fixture.replace("\n", "\r\n"));
        assertNoErrors("CRLF context declarations", crlf.errors);
        assertModifierOwnership(crlf, 9, 5, 4);
        assertAnnotatedContextParameters(crlf, 2);

        for (String source : List.of(
            "context(: Logger) fun invalid() {}",
            "context(logger:) fun invalid() {}",
            "context(logger: Logger = value) fun invalid() {}",
            "context(logger: Logger,, other: Other) fun invalid() {}",
            "context(logger: Logger)",
            "context(logger: Logger)\nprintln(1)",
            "typealias ContextFunction = context(Logger) () -> Unit")) {
            assertHasErrors("invalid context declaration " + source, parse(source).errors);
        }

        System.out.println("Context parameter checks passed.");
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

    private static void assertModifierOwnership(ParseResult result, int contexts, int functions, int properties) {
        assertCount(result, "contextModifier", contexts);
        assertCount(result, "functionModifierList", functions);
        assertCount(result, "propertyModifierList", properties);
        for (ParserRuleContext context : findRules(result.tree, result.parser, "contextModifier")) {
            if (!(context.getParent() instanceof ParserRuleContext parent)) {
                throw new AssertionError("contextModifier has no modifier-list parent");
            }
            String parentRule = ruleName(parent, result.parser);
            if (!parentRule.equals("functionModifierList") && !parentRule.equals("propertyModifierList")) {
                throw new AssertionError("contextModifier belongs to unexpected " + parentRule + ": " + context.getText());
            }
        }
    }

    private static void assertAnnotatedContextParameters(ParseResult result, int expected) {
        int annotated = 0;
        for (ParserRuleContext contextParameter : findRules(result.tree, result.parser, "contextParameter")) {
            int annotations = directRuleChildren(contextParameter, result.parser, "annotations").size();
            if (annotations > 0) {
                annotated++;
            }
        }
        if (annotated != expected) {
            throw new AssertionError("Expected " + expected + " individually annotated context parameters but found " + annotated);
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
        if (tree instanceof ParserRuleContext context && ruleName(context, parser).equals(rule)) {
            result.add(context);
        }
        for (int i = 0; i < tree.getChildCount(); i++) {
            collectRules(tree.getChild(i), parser, rule, result);
        }
    }

    private static List<ParserRuleContext> directRuleChildren(ParserRuleContext context, Parser parser, String rule) {
        List<ParserRuleContext> result = new ArrayList<>();
        for (int i = 0; i < context.getChildCount(); i++) {
            if (context.getChild(i) instanceof ParserRuleContext child && ruleName(child, parser).equals(rule)) {
                result.add(child);
            }
        }
        return result;
    }

    private static String ruleName(ParserRuleContext context, Parser parser) {
        return parser.getRuleNames()[context.getRuleIndex()];
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
