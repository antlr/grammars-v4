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

final class MultiplatformModifierCheck {
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
        String fixture = Files.readString(Path.of("examples/kotlinFile/multiplatform_modifiers.kt"));
        ParseResult result = parse(fixture);
        assertNoErrors("multiplatform modifier fixture", result.errors);
        assertCount(result, "platformModifier", 16);
        assertCount(result, "typeAlias", 2);
        assertPlatformModifierOwnership(result);

        ParseResult crlf = parse(fixture.replace("\n", "\r\n"));
        assertNoErrors("CRLF multiplatform modifier fixture", crlf.errors);
        assertCount(crlf, "platformModifier", 16);
        assertPlatformModifierOwnership(crlf);

        assertNoErrors("expect and actual soft-keyword identifiers", parse(
            "val expect = 1\nval actual = expect\nfun names(expect: Int, actual: Int) = expect + actual\n"
                + "val expectation = actual\nval actualized = expectation\nval `expect` = `actual`\n"
                + "val values = (expect + actual)\nval collection = [expect, actual]\n"
                + "val ordinary = \"${expect + actual}\"\nval raw = \"\"\"${expect + actual}\"\"\"\n").errors);
        assertKeywordTokens("expect actual", List.of("EXPECT", "ACTUAL"));
        assertKeywordTokens("expectation actualized `expect` `actual`", List.of(
            "Identifier", "Identifier", "Identifier", "Identifier"));
        assertKeywordTokens("(expect, actual)", List.of("LPAREN", "EXPECT", "COMMA", "ACTUAL", "RPAREN"));
        assertKeywordTokens("[expect, actual]", List.of("LSQUARE", "EXPECT", "COMMA", "ACTUAL", "RSQUARE"));
        assertKeywordTokens("\"${expect + actual}\"", List.of(
            "QUOTE_OPEN", "LineStrExprStart", "EXPECT", "ADD", "ACTUAL", "RCURL", "QUOTE_CLOSE"));
        assertKeywordTokens("\"\"\"${expect + actual}\"\"\"", List.of(
            "TRIPLE_QUOTE_OPEN", "MultiLineStrExprStart", "EXPECT", "ADD", "ACTUAL", "RCURL", "TRIPLE_QUOTE_CLOSE"));

        for (String source : List.of(
            "expect",
            "actual fun missing(",
            "expect package sample",
            "actual package sample")) {
            assertHasErrors("invalid multiplatform declaration " + source, parse(source).errors);
        }

        System.out.println("Multiplatform modifier checks passed.");
    }

    private static void assertPlatformModifierOwnership(ParseResult result) {
        List<ParserRuleContext> modifiers = findRules(result.tree, result.parser, "platformModifier");
        for (ParserRuleContext platformModifier : modifiers) {
            ParseTree parent = platformModifier.getParent();
            if (!(parent instanceof ParserRuleContext modifier) || !ruleName(modifier, result.parser).equals("modifier")) {
                throw new AssertionError("platformModifier is not owned by modifier: " + platformModifier.getText());
            }
            ParseTree grandparent = modifier.getParent();
            ParserRuleContext modifierList;
            if (grandparent instanceof ParserRuleContext classDeclarationModifier
                && ruleName(classDeclarationModifier, result.parser).equals("classDeclarationModifier")) {
                ParseTree classModifierList = classDeclarationModifier.getParent();
                if (!(classModifierList instanceof ParserRuleContext list)
                    || !ruleName(list, result.parser).equals("classDeclarationModifierList")) {
                    throw new AssertionError("platformModifier is not contained in classDeclarationModifierList: "
                        + platformModifier.getText());
                }
                modifierList = list;
            } else if (grandparent instanceof ParserRuleContext list
                && List.of("modifierList", "functionModifierList", "propertyModifierList")
                    .contains(ruleName(list, result.parser))) {
                modifierList = list;
            } else {
                throw new AssertionError("platformModifier is not contained in a declaration modifierList: "
                    + platformModifier.getText());
            }
            ParseTree declaration = modifierList.getParent();
            if (!(declaration instanceof ParserRuleContext context)
                || !List.of("classDeclaration", "functionDeclaration", "propertyDeclaration", "objectDeclaration",
                    "typeAlias", "primaryConstructor", "secondaryConstructor").contains(ruleName(context, result.parser))) {
                throw new AssertionError("Unexpected platform modifier declaration owner: " + declaration);
            }
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