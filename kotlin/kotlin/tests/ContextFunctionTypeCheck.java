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

final class ContextFunctionTypeCheck {
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
        String fixture = Files.readString(Path.of("examples/kotlinFile/context_function_types.kt"));
        ParseResult result = parseFile(fixture);
        assertNoErrors("context function type fixture", result.errors);
        assertContextFunctionTypes(result, 13, 16);
        assertNamedFunctionTypeParameters(result, 1);
        assertDeclarationContextsRemainSeparate(result, 1, 1);

        ParseResult crlf = parseFile(fixture.replace("\n", "\r\n"));
        assertNoErrors("CRLF context function type fixture", crlf.errors);
        assertContextFunctionTypes(crlf, 13, 16);

        ParseResult script = parseScript("typealias ScriptContext = context(Logger, @Marker Service,) (input: Int) -> String\n"
            + "val operation: context(Logger) suspend (Int) -> String = value\n"
            + "\"${consume<context(Logger) () -> Unit> { it }}\"\n");
        assertNoErrors("context function types in script", script.errors);
        assertContextFunctionTypes(script, 3, 4);
        assertNamedFunctionTypeParameters(script, 1);

        for (String source : List.of(
            "typealias Valid = context(Logger, Service,) () -> Unit",
            "typealias Valid = context(@Marker Logger, List<String?>) Receiver.(Int,) -> Unit",
            "typealias Valid = suspend context(Logger) (Int) -> Unit",
            "typealias Valid = context(Logger) suspend () -> Unit",
            "typealias Valid = context(Logger) ((Int) -> String) -> Unit",
            "fun valid(value: Any) = value is context(Logger) () -> Unit",
            "fun valid(value: Any) = value as context(Logger) () -> Unit",
            "fun valid() = \"${consume<context(Logger) () -> Unit> { it }}\"")) {
            ParseResult valid = parseFile(source);
            assertNoErrors("valid context function type " + source, valid.errors);
            if (findRules(valid.tree, valid.parser, "contextFunctionTypeParameters").isEmpty()) {
                throw new AssertionError("contextFunctionTypeParameters node missing for " + source);
            }
        }

        for (String source : List.of(
            "typealias Invalid = context() () -> Unit",
            "typealias Invalid = context(, Logger) () -> Unit",
            "typealias Invalid = context(logger: Logger) () -> Unit",
            "typealias Invalid = context(Logger, service: Service) () -> Unit",
            "typealias Invalid = context(Logger)",
            "typealias Invalid = context(Logger) String",
            "typealias Invalid = context(Logger, , Service) () -> Unit")) {
            assertHasErrors("invalid context function type " + source, parseFile(source).errors);
        }

        System.out.println("Context function type checks passed.");
    }

    private static void assertContextFunctionTypes(ParseResult result, int expectedLists, int expectedTypes) {
        List<ParserRuleContext> lists = findRules(result.tree, result.parser, "contextFunctionTypeParameters");
        if (lists.size() != expectedLists) {
            throw new AssertionError("Expected " + expectedLists + " context function type parameter lists but found "
                + lists.size());
        }
        int actualTypes = 0;
        for (ParserRuleContext list : lists) {
            if (!(list.getParent() instanceof ParserRuleContext parent)
                    || !ruleName(parent, result.parser).equals("functionType")) {
                throw new AssertionError("Context type entries must belong to functionType: " + list.getText());
            }
            int directTypes = directRuleChildren(list, result.parser, "type").size();
            if (directTypes == 0) {
                throw new AssertionError("A context function type must have at least one type entry: " + list.getText());
            }
            actualTypes += directTypes;
            if (!directRuleChildren(list, result.parser, "parameter").isEmpty()
                    || !directRuleChildren(list, result.parser, "contextParameter").isEmpty()) {
                throw new AssertionError("Context function type entries must not be declaration parameters: "
                    + list.getText());
            }
        }
        if (actualTypes != expectedTypes) {
            throw new AssertionError("Expected " + expectedTypes + " context entry types but found " + actualTypes);
        }
    }

    private static void assertNamedFunctionTypeParameters(ParseResult result, int expected) {
        int actual = 0;
        for (ParserRuleContext parameters : findRules(result.tree, result.parser, "functionTypeParameters")) {
            actual += directRuleChildren(parameters, result.parser, "parameter").size();
        }
        if (actual != expected) {
            throw new AssertionError("Expected " + expected + " named ordinary function-type parameters but found "
                + actual);
        }
    }

    private static void assertDeclarationContextsRemainSeparate(ParseResult result, int expectedLists,
        int expectedParameters) {
        assertCount(result, "contextParameters", expectedLists);
        assertCount(result, "contextParameter", expectedParameters);
        for (ParserRuleContext context : findRules(result.tree, result.parser, "contextParameter")) {
            if (!(context.getParent() instanceof ParserRuleContext parent)
                    || !ruleName(parent, result.parser).equals("contextParameters")) {
                throw new AssertionError("Declaration context parameter ownership changed: " + context.getText());
            }
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

    private static ParseResult parseFile(String source) {
        return parse(source, false);
    }

    private static ParseResult parseScript(String source) {
        return parse(source, true);
    }

    private static ParseResult parse(String source, boolean script) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        return new ParseResult(parser, script ? parser.script() : parser.kotlinFile(), errors);
    }

    private static BaseErrorListener errorCollector(List<String> errors) {
        return new BaseErrorListener() {
            @Override
            public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line,
                int charPositionInLine, String message, RecognitionException exception) {
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