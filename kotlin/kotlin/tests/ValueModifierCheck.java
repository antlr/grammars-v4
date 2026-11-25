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

final class ValueModifierCheck {
    private static final class ParseResult {
        private final KotlinParser parser;
        private final ParserRuleContext tree;
        private final List<String> errors;

        private ParseResult(KotlinParser parser, ParserRuleContext tree, List<String> errors) {
            this.parser = parser;
            this.tree = tree;
            this.errors = errors;
        }
    }

    public static void main(String[] args) throws IOException {
        String fixture = Files.readString(Path.of("examples/kotlinFile/value_classes.kt"));
        ParseResult result = parseFile(fixture);
        assertNoErrors("value class fixture", result.errors);
        assertValueClassModifiers(result, 7);

        ParseResult crlf = parseFile(fixture.replace("\n", "\r\n"));
        assertNoErrors("CRLF value class fixture", crlf.errors);
        assertValueClassModifiers(crlf, 7);

        ParseResult script = parseScript("value class ScriptId(val value: Int)\nval value = ScriptId(1)\n");
        assertNoErrors("value class in script", script.errors);
        assertValueClassModifiers(script, 1);

        assertNoErrors("value as a soft-keyword identifier", parseFile(
            "val value = 1\nval valueClass = value + 1\nval `value` = valueClass\n"
                + "val nested = (value)\nval indexed = [value]\n"
                + "val ordinary = \"${value}\"\nval raw = \"\"\"${value}\"\"\"\n").errors);
        assertKeywordTokens("value valueClass `value`", List.of("VALUE", "Identifier", "Identifier"));
        assertKeywordTokens("(value)", List.of("LPAREN", "VALUE", "RPAREN"));
        assertKeywordTokens("[value]", List.of("LSQUARE", "VALUE", "RSQUARE"));
        assertKeywordTokens("\"${value}\"", List.of(
            "QUOTE_OPEN", "LineStrExprStart", "VALUE", "RCURL", "QUOTE_CLOSE"));
        assertKeywordTokens("\"\"\"${value}\"\"\"", List.of(
            "TRIPLE_QUOTE_OPEN", "MultiLineStrExprStart", "VALUE", "RCURL", "TRIPLE_QUOTE_CLOSE"));

        for (String source : List.of(
            "value class",
            "value object Unsupported",
            "value interface Unsupported",
            "value fun interface Unsupported",
            "value class 1")) {
            assertHasErrors("invalid value declaration " + source, parseFile(source).errors);
        }

        System.out.println("Value modifier checks passed.");
    }

    private static void assertValueClassModifiers(ParseResult result, int expected) {
        List<ParserRuleContext> classModifiers = findRules(result.tree, result.parser, "classModifier");
        int actual = 0;
        for (ParserRuleContext classModifier : classModifiers) {
            if (!classModifier.getText().equals("value")) {
                continue;
            }
            actual++;
            ParseTree parent = classModifier.getParent();
            if (!(parent instanceof ParserRuleContext modifier)
                || !ruleName(modifier, result.parser).equals("classDeclarationModifier")) {
                throw new AssertionError("value class modifier is not owned by classDeclarationModifier: "
                    + classModifier.getText());
            }
            ParseTree modifierList = modifier.getParent();
            if (!(modifierList instanceof ParserRuleContext list)
                || !ruleName(list, result.parser).equals("classDeclarationModifierList")) {
                throw new AssertionError("value class modifier is not contained in classDeclarationModifierList: "
                    + classModifier);
            }
            ParseTree declaration = list.getParent();
            if (!(declaration instanceof ParserRuleContext context)
                || !ruleName(context, result.parser).equals("classDeclaration")) {
                throw new AssertionError("value modifier is not attached to classDeclaration: " + declaration);
            }
        }
        if (actual != expected) {
            throw new AssertionError("Expected " + expected + " value class modifiers but found " + actual);
        }
    }

    private static void assertKeywordTokens(String source, List<String> expected) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> actual = new ArrayList<>();
        for (Token token = lexer.nextToken(); token.getType() != Token.EOF; token = lexer.nextToken()) {
            actual.add(lexer.getVocabulary().getSymbolicName(token.getType()));
        }
        if (!actual.equals(expected)) {
            throw new AssertionError("Expected tokens " + expected + " for " + source + " but found " + actual);
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