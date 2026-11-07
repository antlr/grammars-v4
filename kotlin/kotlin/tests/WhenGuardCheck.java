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

final class WhenGuardCheck {
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
        String fixture = Files.readString(Path.of("examples/kotlinFile/when_guards.kt"));
        ParseResult result = parseFile(fixture);
        assertNoErrors("when guard fixture", result.errors);
        assertGuardOwnership(result, 7);

        ParseResult crlf = parseFile(fixture.replace("\n", "\r\n"));
        assertNoErrors("CRLF when guard fixture", crlf.errors);
        assertGuardOwnership(crlf, 7);

        ParseResult script = parseScript("val result = when (value) {\n"
            + "    is String if /* guard comment */\n value.isNotEmpty() -> value\n"
            + "    else if value == null -> \"null\"\n"
            + "    else -> \"other\"\n}\n");
        assertNoErrors("when guards in script", script.errors);
        assertGuardOwnership(script, 2);

        for (String source : List.of(
            "fun valid(value: Any) = when (value) { is String if value.isNotEmpty() -> value; else -> \"other\" }",
            "fun valid(value: Any) = when (value) { in values if enabled -> 1; else -> 0 }",
            "fun valid(value: Any) = when (value) { 1, 2 if enabled -> 1; else -> 0 }",
            "fun valid(value: Any) = when (value) { else if enabled -> 1 }",
            "fun valid(value: Any) = when (value) { is String if if (enabled) true else false -> 1; else -> 0 }",
            "fun valid(value: Any) = \"${when (value) { is String if value.isNotEmpty() -> value; else -> \"other\" }}\"")) {
            ParseResult valid = parseFile(source);
            assertNoErrors("valid guarded when " + source, valid.errors);
            if (findRules(valid.tree, valid.parser, "whenGuard").isEmpty()) {
                throw new AssertionError("whenGuard node missing for " + source);
            }
        }

        for (String source : List.of(
            "fun invalid(value: Any) = when (value) { is String if -> 1; else -> 0 }",
            "fun invalid(value: Any) = when (value) { is String if true if false -> 1; else -> 0 }",
            "fun invalid(value: Any) = when (value) { else if -> 1 }",
            "fun invalid(value: Any) = when (value) { is String if true 1; else -> 0 }")) {
            assertHasErrors("invalid when guard " + source, parseFile(source).errors);
        }

        System.out.println("When guard checks passed.");
    }

    private static void assertGuardOwnership(ParseResult result, int expectedGuards) {
        List<ParserRuleContext> entries = findRules(result.tree, result.parser, "whenEntry");
        int actualGuards = 0;
        for (ParserRuleContext entry : entries) {
            List<ParserRuleContext> guards = directRuleChildren(entry, result.parser, "whenGuard");
            actualGuards += guards.size();
            for (ParserRuleContext guard : guards) {
                if (directRuleChildren(guard, result.parser, "expression").size() != 1
                        || countDirectTokens(guard, result.parser, "IF") != 1) {
                    throw new AssertionError("Each whenGuard must own one IF token and one expression: "
                        + guard.getText());
                }
            }
        }
        if (actualGuards != expectedGuards) {
            throw new AssertionError("Expected " + expectedGuards + " guards directly under whenEntry but found "
                + actualGuards);
        }
    }

    private static int countDirectTokens(ParserRuleContext context, Parser parser, String tokenName) {
        int count = 0;
        for (int i = 0; i < context.getChildCount(); i++) {
            if (context.getChild(i) instanceof org.antlr.v4.runtime.tree.TerminalNode terminal
                    && parser.getVocabulary().getSymbolicName(terminal.getSymbol().getType()).equals(tokenName)) {
                count++;
            }
        }
        return count;
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