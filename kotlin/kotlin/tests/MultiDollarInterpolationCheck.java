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

final class MultiDollarInterpolationCheck {
    private static final class ParseResult {
        private final Parser parser;
        private final ParserRuleContext tree;
        private final List<? extends Token> tokens;
        private final List<String> errors;

        private ParseResult(Parser parser, ParserRuleContext tree, List<? extends Token> tokens, List<String> errors) {
            this.parser = parser;
            this.tree = tree;
            this.tokens = tokens;
            this.errors = errors;
        }
    }

    public static void main(String[] args) throws IOException {
        String fixture = Files.readString(Path.of("examples/kotlinFile/multi_dollar_interpolation.kt"));
        ParseResult result = parseFile(fixture);
        assertNoErrors("multi-dollar fixture", result.errors);
        assertToken(result, "QUOTE_OPEN", "$\"");
        assertToken(result, "QUOTE_OPEN", "$$\"");
        assertToken(result, "QUOTE_OPEN", "$$$\"");
        assertToken(result, "QUOTE_OPEN", "$$$$\"");
        assertToken(result, "TRIPLE_QUOTE_OPEN", "$\"\"\"");
        assertToken(result, "TRIPLE_QUOTE_OPEN", "$$\"\"\"");
        assertToken(result, "TRIPLE_QUOTE_OPEN", "$$$\"\"\"");
        assertToken(result, "TRIPLE_QUOTE_OPEN", "$$$$\"\"\"");
        assertToken(result, "LineStrRef", "$name");
        assertToken(result, "LineStrRef", "$$name");
        assertToken(result, "LineStrRef", "$$$name");
        assertToken(result, "LineStrRef", "$$$$name");
        assertToken(result, "MultiLineStrRef", "$$$name");
        assertToken(result, "MultiLineStrRef", "$name");
        assertToken(result, "LineStrExprStart", "${");
        assertToken(result, "LineStrExprStart", "$${");
        assertToken(result, "LineStrExprStart", "$$$${");
        assertToken(result, "MultiLineStrExprStart", "$$${");
        assertToken(result, "MultiLineStrExprStart", "${");
        assertToken(result, "MultiLineStrExprStart", "$$$${");
        assertToken(result, "LineStrText", "$");
        assertToken(result, "MultiLineStrText", "$");
        assertToken(result, "LineStrEscapedChar", "\\$");
        assertRuleCount(result, "lineStringExpression", 10);
        assertRuleCount(result, "multiLineStringExpression", 6);

        ParseResult crlf = parseFile(fixture.replace("\n", "\r\n"));
        assertNoErrors("CRLF multi-dollar fixture", crlf.errors);

        for (String lineEnding : List.of("\r", "\n", "\r\n")) {
            ParseResult raw = parseFile("val text = $$$\"\"\"first" + lineEnding + "second" + lineEnding + "third\"\"\"");
            assertNoErrors("raw multi-dollar string with " + printable(lineEnding), raw.errors);
            for (Token token : raw.tokens) {
                if ("NL".equals(symbolicName(raw, token))) {
                    throw new AssertionError("Raw-string line endings must remain string content: " + printable(lineEnding));
                }
            }
        }

        ParseResult unsupported = parseFile("val text = $$$$$\"unsupported\"");
        assertHasErrors("five-dollar string prefix", unsupported.errors);

        System.out.println("Multi-dollar interpolation checks passed.");
    }

    private static void assertToken(ParseResult result, String name, String text) {
        for (Token token : result.tokens) {
            if (name.equals(symbolicName(result, token)) && text.equals(token.getText())) {
                return;
            }
        }
        throw new AssertionError("Missing " + name + " token with text " + printable(text));
    }

    private static void assertRuleCount(ParseResult result, String rule, int expected) {
        int actual = countRules(result.tree, result.parser, rule);
        if (actual != expected) {
            throw new AssertionError("Expected " + expected + " " + rule + " nodes but found " + actual);
        }
    }

    private static int countRules(ParseTree tree, Parser parser, String rule) {
        int count = tree instanceof ParserRuleContext context && ruleName(context, parser).equals(rule) ? 1 : 0;
        for (int i = 0; i < tree.getChildCount(); i++) {
            count += countRules(tree.getChild(i), parser, rule);
        }
        return count;
    }

    private static String ruleName(ParserRuleContext context, Parser parser) {
        return parser.getRuleNames()[context.getRuleIndex()];
    }

    private static String symbolicName(ParseResult result, Token token) {
        return result.parser.getVocabulary().getSymbolicName(token.getType());
    }

    private static ParseResult parseFile(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        tokens.fill();
        tokens.seek(0);
        KotlinParser parser = new KotlinParser(tokens);
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        ParserRuleContext tree = parser.kotlinFile();
        List<? extends Token> tokenList = tokens.getTokens();
        return new ParseResult(parser, tree, tokenList, errors);
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
            throw new AssertionError(label + " should produce a lexer or parser diagnostic");
        }
    }

    private static String printable(String value) {
        return value.replace("\r", "\\r").replace("\n", "\\n");
    }
}