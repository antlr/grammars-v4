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

final class NamedDeclarationCheck {
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
        String fixture = Files.readString(Path.of("examples/kotlinFile/named_declarations.kt"));
        ParseResult result = parse(fixture);
        assertNoErrors("named declaration fixture", result.errors);
        assertFunctionNamesAndReceivers(result, List.of(
            new FunctionShape("ordinary", null),
            new FunctionShape("member", "a.b"),
            new FunctionShape("generic", "T"),
            new FunctionShape("boxed", "Box<String>"),
            new FunctionShape("nullable", "String?"),
            new FunctionShape("annotated", "@ReceiverMarkerString"),
            new FunctionShape("parenthesized", "(String)"),
            new FunctionShape("`name.with.dot`", null),
            new FunctionShape("semanticModifier", null),
            new FunctionShape("objectExpression", null)));
        assertCount(result, "objectDeclaration", 2);
        assertCount(result, "objectLiteral", 1);
        assertCount(result, "primaryConstructor", 1);
        assertDirectChildCount(result, "objectDeclaration", "primaryConstructor", 0);

        ParseResult crlf = parse(fixture.replace("\n", "\r\n"));
        assertNoErrors("CRLF named declaration fixture", crlf.errors);
        assertFunctionNamesAndReceivers(crlf, List.of(
            new FunctionShape("ordinary", null),
            new FunctionShape("member", "a.b"),
            new FunctionShape("generic", "T"),
            new FunctionShape("boxed", "Box<String>"),
            new FunctionShape("nullable", "String?"),
            new FunctionShape("annotated", "@ReceiverMarkerString"),
            new FunctionShape("parenthesized", "(String)"),
            new FunctionShape("`name.with.dot`", null),
            new FunctionShape("semanticModifier", null),
            new FunctionShape("objectExpression", null)));

        for (String source : List.of(
            "fun simple() {}",
            "fun receiver.member() {}",
            "fun receiver.namespace.member() {}",
            "fun `name.with.dot`() {}",
            "class WithPrimary(val value: Int)",
            "object Named",
            "object Extends : Base()",
            "object Delegates : Runnable by handler",
            "class Host { companion object NamedCompanion }",
            "fun expression() { val local = object : Base() {} }",
            "open fun compilerCheckedModifier() {}",
            "package valid.name\nimport sample.Type\nfun declaration() {}")) {
            assertNoErrors("valid named declaration source " + source, parse(source).errors);
        }

        for (String source : List.of(
            "fun () {}",
            "fun receiver.() {}",
            "object Named()",
            "public package invalid.name",
            "open package invalid.name",
            "@Deprecated package invalid.name",
            "@Deprecated package invalid.name\nfun declaration() {}",
            "@Deprecated package invalid.name\nval value = 1")) {
            assertHasErrors("invalid named declaration source " + source, parse(source).errors);
        }

        for (String source : List.of(
            "package valid.script\nprintln(1)",
            "@file:Deprecated\npackage valid.script\nprintln(1)")) {
            assertNoErrors("valid package header in script " + source, parseScript(source).errors);
        }

        for (String source : List.of(
            "private package invalid.script\nprintln(1)",
            "open package invalid.script\nprintln(1)",
            "@Deprecated package invalid.script\nprintln(1)")) {
            assertHasErrors("invalid package header in script " + source, parseScript(source).errors);
        }

        System.out.println("Named declaration checks passed.");
    }

    private static final class FunctionShape {
        private final String name;
        private final String receiver;

        private FunctionShape(String name, String receiver) {
            this.name = name;
            this.receiver = receiver;
        }
    }

    private static void assertFunctionNamesAndReceivers(ParseResult result, List<FunctionShape> expected) {
        List<ParserRuleContext> functions = findRules(result.tree, result.parser, "functionDeclaration");
        if (functions.size() != expected.size()) {
            throw new AssertionError("Expected " + expected.size() + " function declarations but found " + functions.size());
        }
        for (int i = 0; i < functions.size(); i++) {
            ParserRuleContext function = functions.get(i);
            FunctionShape shape = expected.get(i);
            List<ParserRuleContext> names = directRuleChildren(function, result.parser, "simpleIdentifier");
            List<ParserRuleContext> receivers = directRuleChildren(function, result.parser, "receiverType");
            if (names.size() != 1 || !names.get(0).getText().equals(shape.name)) {
                throw new AssertionError("Expected required simple function name " + shape.name + " but found "
                    + names.stream().map(ParseTree::getText).toList());
            }
            if (!directRuleChildren(function, result.parser, "identifier").isEmpty()) {
                throw new AssertionError("Function name was parsed as a qualified identifier: " + function.getText());
            }
            if (shape.receiver == null) {
                if (!receivers.isEmpty()) {
                    throw new AssertionError("Unexpected extension receiver for " + shape.name + ": " + receivers);
                }
            } else if (receivers.size() != 1 || !receivers.get(0).getText().equals(shape.receiver)) {
                throw new AssertionError("Expected receiver " + shape.receiver + " for " + shape.name + " but found "
                    + receivers.stream().map(ParseTree::getText).toList());
            }
        }
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

    private static ParseResult parseScript(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        return new ParseResult(parser, parser.script(), errors);
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
