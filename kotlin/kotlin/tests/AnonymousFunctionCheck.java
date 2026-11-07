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

final class AnonymousFunctionCheck {
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
        String fixture = Files.readString(Path.of("examples/kotlinFile/anonymous_functions.kt"));
        ParseResult file = parseFile(fixture);
        assertNoErrors("anonymous function fixture", file.errors);
        assertAnonymousFunctionOwnership(file, 9);
        if (findRules(file.tree, file.parser, "functionDeclaration").size() != 8) {
            throw new AssertionError("Anonymous functions must not become named function declarations");
        }
        if (!findRules(file.tree, file.parser, "functionLiteral").isEmpty()) {
            throw new AssertionError("Anonymous functions must not be parsed as brace function literals");
        }

        ParseResult crlf = parseFile(fixture.replace("\n", "\r\n"));
        assertNoErrors("CRLF anonymous function fixture", crlf.errors);
        assertAnonymousFunctionOwnership(crlf, 9);

        ParseResult script = parseScript("val operation = fun(value: Int) = value + 1\noperation(2)\n");
        assertNoErrors("anonymous function script", script.errors);
        assertAnonymousFunctionOwnership(script, 1);

        for (String source : List.of(
            "fun valid() = fun() { 1 }",
            "fun valid() = fun(value: Int,) = value",
            "fun valid() = fun(value) = value",
            "fun valid() = fun String.(value: Int): String = this",
            "fun valid() = consume(fun(value: Int) = value)",
            "fun valid() = (fun(value: Int) = value)(1)",
            "fun valid() = \"${fun(value: Int) = value}\"")) {
            ParseResult valid = parseFile(source);
            assertNoErrors("valid anonymous function " + source, valid.errors);
            if (findRules(valid.tree, valid.parser, "anonymousFunction").isEmpty()) {
                throw new AssertionError("anonymousFunction node missing for " + source);
            }
        }

        for (String source : List.of(
            "fun invalid() = fun .(value: Int) = value",
            "fun invalid() = fun(value: ) = value",
            "fun invalid() = fun(value: Int): = value",
            "fun invalid() = fun(value: Int) =",
            "fun invalid() = fun(value: Int,,) = value",
            "val invalid = fun named(value: Int) = value",
            "fun invalid() = consume fun(value: Int) = value")) {
            assertHasErrors("invalid anonymous function " + source, parseFile(source).errors);
        }

        System.out.println("Anonymous function checks passed.");
    }

    private static void assertAnonymousFunctionOwnership(ParseResult result, int expectedFunctions) {
        List<ParserRuleContext> functions = findRules(result.tree, result.parser, "anonymousFunction");
        if (functions.size() != expectedFunctions) {
            throw new AssertionError("Expected " + expectedFunctions + " anonymousFunction nodes but found "
                + functions.size());
        }
        for (ParserRuleContext function : functions) {
            if (!(function.getParent() instanceof ParserRuleContext parent)
                    || !ruleName(parent, result.parser).equals("atomicExpression")) {
                throw new AssertionError("anonymousFunction must be an atomic expression: " + function.getText());
            }
            if (directRuleChildren(function, result.parser, "anonymousFunctionParameters").size() != 1) {
                throw new AssertionError("anonymousFunction must own its parameter list: " + function.getText());
            }
            if (directRuleChildren(function, result.parser, "functionBody").size() != 1) {
                throw new AssertionError("anonymousFunction must own its body: " + function.getText());
            }
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
        ParserRuleContext tree = script ? parser.script() : parser.kotlinFile();
        return new ParseResult(parser, tree, errors);
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