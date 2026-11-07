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
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

final class CallableReferenceCheck {
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
        ParseResult fixture = parseFile(Files.readString(Path.of("examples/kotlinFile/callable_references.kt")));
        assertNoErrors("callable reference fixture", fixture.errors);

        for (String expression : List.of(
                 "factory()::run",
                 "(value)::member",
                 "this::member",
                 "factory()::class",
                 "this@Owner::member",
                 "values[0]::member",
                 "nullable!!::member",
                 "value.member::nested",
                 "factory()::`when`",
                 "factory()::inline",
                 "factory()::run.invoke()",
                 "factory()::run.member",
                 "factory()::run?.member",
                 "(value)::member.invoke()",
                 "::factory",
                 "String::length",
                 "List<String>::size",
                 "String?::length",
                 "this::class")) {
            assertNoErrors("callable reference " + expression, parseExpression(expression).errors);
        }

        assertReferenceSuffixAndFollowingNavigation();
        assertReferenceSuffixTarget("factory()::run", "run");
        assertReferenceSuffixTarget("factory()::`when`", "`when`");
        assertReferenceSuffixTarget("factory()::class", "class");

        for (String expression : List.of("factory()::", "factory()::1", "(value)::")) {
            assertHasErrors("invalid callable reference " + expression, parseExpression(expression).errors);
        }
        System.out.println("Callable reference checks passed.");
    }

    private static void assertReferenceSuffixAndFollowingNavigation() {
        ParseResult result = parseExpression("factory()::run.invoke()");
        assertNoErrors("reference followed by navigation", result.errors);
        ParserRuleContext chain = findRuleWithText(result.tree, result.parser, "postfixUnaryExpression", "factory()::run.invoke()");
        if (chain == null) {
            throw new AssertionError("Did not find the full postfix chain for factory()::run.invoke()");
        }

        List<ParserRuleContext> operations = directRuleChildren(chain, result.parser, "postfixUnaryOperation");
        ParserRuleContext reference = null;
        ParserRuleContext navigation = null;
        for (ParserRuleContext operation : operations) {
            if (hasDirectToken(operation, KotlinParser.COLONCOLON)) {
                reference = operation;
            }
            if (!directRuleChildren(operation, result.parser, "memberAccessOperator").isEmpty()) {
                navigation = operation;
            }
        }
        assertReferenceTarget(result.parser, reference, "run");
        if (navigation == null) {
            throw new AssertionError("Navigation after ::run must remain a separate postfix operation: " + chain.getText());
        }
        if (!directRuleChildren(navigation, result.parser, "postfixUnaryExpression").isEmpty()) {
            throw new AssertionError("Navigation selector must not recursively own a postfix expression: " + navigation.getText());
        }
        List<ParserRuleContext> selectors = directRuleChildren(navigation, result.parser, "simpleIdentifier");
        if (selectors.size() != 1 || !selectors.get(0).getText().equals("invoke")) {
            throw new AssertionError("Expected invoke as the direct navigation selector but found " + navigation.getText());
        }
        if (directRuleChildren(chain, result.parser, "postfixCallChain").size() != 2) {
            throw new AssertionError("Calls before and after navigation must belong to the outer postfix expression: "
                + chain.getText());
        }
    }

    private static void assertReferenceSuffixTarget(String expression, String target) {
        ParseResult result = parseExpression(expression);
        assertNoErrors("reference suffix " + expression, result.errors);
        ParserRuleContext operation = null;
        for (ParserRuleContext candidate : findRules(result.tree, result.parser, "postfixUnaryOperation")) {
            if (hasDirectToken(candidate, KotlinParser.COLONCOLON)) {
                operation = candidate;
                break;
            }
        }
        assertReferenceTarget(result.parser, operation, target);
    }

    private static void assertReferenceTarget(Parser parser, ParserRuleContext operation, String target) {
        if (operation == null) {
            throw new AssertionError("Expected a :: postfix operation");
        }
        if (target.equals("class")) {
            if (!hasDirectToken(operation, KotlinParser.CLASS)) {
                throw new AssertionError("Expected ::class target, found " + operation.getText());
            }
            return;
        }
        List<ParserRuleContext> identifiers = directRuleChildren(operation, parser, "simpleIdentifier");
        if (identifiers.size() != 1 || !identifiers.get(0).getText().equals(target)) {
            throw new AssertionError("Expected :: target " + target + " as a simpleIdentifier, found " + operation.getText());
        }
    }

    private static ParseResult parseFile(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        return new ParseResult(parser, parser.kotlinFile(), errors);
    }

    private static ParseResult parseExpression(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        ParserRuleContext tree = parser.expression();
        if (parser.getCurrentToken().getType() != Token.EOF) {
            errors.add("Unconsumed input " + parser.getCurrentToken().getText());
        }
        return new ParseResult(parser, tree, errors);
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

    private static ParserRuleContext findRuleWithText(ParseTree tree, Parser parser, String rule, String text) {
        for (ParserRuleContext context : findRules(tree, parser, rule)) {
            if (context.getText().equals(text)) {
                return context;
            }
        }
        return null;
    }

    private static List<ParserRuleContext> directRuleChildren(ParserRuleContext context, Parser parser, String rule) {
        List<ParserRuleContext> result = new ArrayList<>();
        if (context == null) {
            return result;
        }
        for (int i = 0; i < context.getChildCount(); i++) {
            if (context.getChild(i) instanceof ParserRuleContext child && ruleName(child, parser).equals(rule)) {
                result.add(child);
            }
        }
        return result;
    }

    private static boolean hasDirectToken(ParserRuleContext context, int tokenType) {
        for (int i = 0; i < context.getChildCount(); i++) {
            if (context.getChild(i) instanceof TerminalNode terminal && terminal.getSymbol().getType() == tokenType) {
                return true;
            }
        }
        return false;
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
