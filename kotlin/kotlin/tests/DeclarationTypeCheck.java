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

final class DeclarationTypeCheck {
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
        String fixture = Files.readString(Path.of("examples/kotlinFile/declaration_types.kt"));
        ParseResult result = parse(fixture);
        assertNoErrors("declaration type fixture", result.errors);
        assertDirectChildCount(result, "delegationSpecifier", "functionType", 2);
        assertDirectChildCount(result, "explicitDelegation", "functionType", 2);
        assertDirectChildCount(result, "catchBlock", "type", 4);
        assertDirectChildCount(result, "setter", "type", 2);
        assertDirectChildCount(result, "typeRHS", "type", 3);
        assertCount(result, "typeOperation", 3);

        ParseResult crlf = parse(fixture.replace("\n", "\r\n"));
        assertNoErrors("CRLF declaration type fixture", crlf.errors);
        assertDirectChildCount(crlf, "delegationSpecifier", "functionType", 2);
        assertDirectChildCount(crlf, "explicitDelegation", "functionType", 2);
        assertDirectChildCount(crlf, "catchBlock", "type", 4);
        assertDirectChildCount(crlf, "setter", "type", 2);

        for (String source : List.of(
            "class FunctionSupertype : () -> Unit",
            "class FunctionDelegation : (String) -> Unit by handler",
            "object FunctionObject : () -> Unit",
            "object FunctionDelegationObject : (Int) -> String by handler",
            "class ExistingSupertype : BaseType(), Named",
            "class ExistingDelegation : Named by delegate",
            "fun catches() { try {} catch (error: (@TypeMarker String) -> Unit) {} }",
            "fun catches() { try {} catch (error: String?,) {} }",
            "var value: Int = 0\n    set(value): Unit { field = value }",
            "var value: Int = 0\n    set(value: Int,): kotlin.Unit = value",
            "var value: Int = 0\n    set(value) { field = value }",
            "fun casts(value: Any) { val result = value as? (String) -> Unit }")) {
            assertNoErrors("valid declaration type source " + source, parse(source).errors);
        }

        for (String source : List.of(
            "class MissingFunctionResult : () ->",
            "class MissingDelegationResult : () -> by handler",
            "fun catches() { try {} catch (error: -> Unit) {} }",
            "var value: Int = 0\n    set(value): { field = value }",
            "fun casts(value: Any) { val result = value as () -> }")) {
            assertHasErrors("invalid declaration type source " + source, parse(source).errors);
        }

        System.out.println("Declaration type checks passed.");
    }

    private static void assertDirectChildCount(ParseResult result, String parent, String child, int expected) {
        int actual = 0;
        for (ParserRuleContext context : findRules(result.tree, result.parser, parent)) {
            actual += directRuleChildren(context, result.parser, child).size();
        }
        if (actual != expected) {
            throw new AssertionError("Expected " + expected + " direct " + child + " children of " + parent
                + " but found " + actual);
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
