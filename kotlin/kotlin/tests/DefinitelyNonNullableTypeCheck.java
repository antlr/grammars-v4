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

final class DefinitelyNonNullableTypeCheck {
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
        String fixture = Files.readString(Path.of("examples/kotlinFile/definitely_non_nullable_types.kt"));
        ParseResult result = parseFile(fixture);
        assertNoErrors("definitely non-nullable type fixture", result.errors);
        int typeCount = findRules(result.tree, result.parser, "definitelyNonNullableType").size();
        if (typeCount < 10) {
            throw new AssertionError("Expected at least 10 dedicated definitelyNonNullableType nodes but found "
                + typeCount);
        }
        assertDirectChildCount(result, "type", "definitelyNonNullableType", typeCount);
        assertDirectChildCount(result, "definitelyNonNullableType", "definitelyNonNullableTypeOperand", typeCount * 2);

        ParseResult crlf = parseFile(fixture.replace("\n", "\r\n"));
        assertNoErrors("CRLF definitely non-nullable type fixture", crlf.errors);
        if (findRules(crlf.tree, crlf.parser, "definitelyNonNullableType").size() != typeCount) {
            throw new AssertionError("CRLF input changed the definitely non-nullable type count");
        }

        ParseResult script = parseScript("fun <T> identity(value: T & Any): T & Any = value\n");
        assertNoErrors("definitely non-nullable type in script", script.errors);
        if (findRules(script.tree, script.parser, "definitelyNonNullableType").size() != 2) {
            throw new AssertionError("Expected parameter and return definitely non-nullable types in script");
        }

        for (String source : List.of(
            "fun <T> valid(value: T & Any): T & Any = value",
            "fun <T> valid(value: T & @Marker Any): T & Any = value",
            "fun <T> valid(value: ((T)) & Any): T & Any = value",
            "fun <T> valid(block: (T & Any) -> T & Any) {}",
            "fun <T> valid(value: Any): T & Any = value as T & Any",
            "fun <T> valid(value: Any) = value is T & Any",
            "fun <T> valid(values: List<out T & Any>) {}",
            "fun <T> (T & Any).valid(): Unit {}",
            "fun <T> valid(value: T\n& Any): T & Any = value")) {
            ParseResult valid = parseFile(source);
            assertNoErrors("valid definitely non-nullable type source " + source, valid.errors);
            if (findRules(valid.tree, valid.parser, "definitelyNonNullableType").isEmpty()) {
                throw new AssertionError("Dedicated type node missing for " + source);
            }
        }

        for (String source : List.of(
            "fun <T> invalid(value: T & Any & Other) {}",
            "fun <T> invalid(value: T? & Any) {}",
            "fun <T> invalid(value: (() -> Unit) & Any) {}",
            "fun <T> invalid(value: Int & Any & Other) {}",
            "fun invalid(value: Int) { val result = value & other }",
            "fun invalid(value: Int & ) {}",
            "fun invalid(value: & Any) {}")) {
            assertHasErrors("invalid definitely non-nullable or intersection source " + source, parseFile(source).errors);
        }

        assertToken("&", "AMP");
        assertToken("&&", "CONJ");
        assertTokens("(T & Any)", List.of("LPAREN", "Identifier", "AMP", "Identifier", "RPAREN"));
        assertTokens("[T & Any]", List.of("LSQUARE", "Identifier", "AMP", "Identifier", "RSQUARE"));
        assertTokens("\"${value as T & Any}\"", List.of(
            "QUOTE_OPEN", "LineStrExprStart", "VALUE", "AS", "Identifier", "AMP", "Identifier",
            "RCURL", "QUOTE_CLOSE"));
        assertTokens("\"\"\"${value as T & Any}\"\"\"", List.of(
            "TRIPLE_QUOTE_OPEN", "MultiLineStrExprStart", "VALUE", "AS", "Identifier", "AMP", "Identifier",
            "RCURL", "TRIPLE_QUOTE_CLOSE"));

        System.out.println("Definitely non-nullable type checks passed.");
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

    private static void assertToken(String source, String expected) {
        assertTokens(source, List.of(expected));
    }

    private static void assertTokens(String source, List<String> expected) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> actual = new ArrayList<>();
        for (Token token = lexer.nextToken(); token.getType() != Token.EOF; token = lexer.nextToken()) {
            actual.add(lexer.getVocabulary().getSymbolicName(token.getType()));
        }
        if (!actual.equals(expected)) {
            throw new AssertionError("Expected tokens " + expected + " for " + source + " but found " + actual);
        }
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