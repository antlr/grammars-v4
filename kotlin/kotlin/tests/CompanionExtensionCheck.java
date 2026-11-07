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

final class CompanionExtensionCheck {
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
        String fixture = Files.readString(Path.of("examples/kotlinFile/companion_extensions.kt"));
        ParseResult result = parseFile(fixture);
        assertNoErrors("companion extension fixture", result.errors);
        assertCompanionModifierOwnership(result, 8);
        assertCount(result, "companionObject", 1);
        assertIdentifierUse(result, "companion", 3);

        for (String lineEnding : List.of("\r", "\n", "\r\n")) {
            ParseResult lineEndingResult = parseFile(fixture.replace("\n", lineEnding));
            assertNoErrors("companion extension fixture with " + printable(lineEnding), lineEndingResult.errors);
            assertCompanionModifierOwnership(lineEndingResult, 8);
        }

        ParseResult script = parseScript(
            "companion fun String.scriptExtension() = this\n"
                + "companion val String.scriptProperty: Int get() = length");
        assertNoErrors("companion extensions in script", script.errors);
        assertCompanionModifierOwnership(script, 2);

        for (String source : List.of(
            "companion fun String.() {}",
            "companion val String.: Int = 0",
            "companion class NotACompanionClass",
            "companion interface NotACompanionInterface")) {
            assertHasErrors("invalid companion declaration " + source, parseFile(source).errors);
        }

        ParseResult companionObject = parseFile("class Host { companion object Named }");
        assertNoErrors("companion object", companionObject.errors);
        assertCount(companionObject, "companionObject", 1);
        assertCount(companionObject, "companionModifier", 0);

        System.out.println("Companion extension checks passed.");
    }

    private static void assertCompanionModifierOwnership(ParseResult result, int expected) {
        List<ParserRuleContext> modifiers = findRules(result.tree, result.parser, "companionModifier");
        if (modifiers.size() != expected) {
            throw new AssertionError("Expected " + expected + " companion modifiers but found " + modifiers.size());
        }
        for (ParserRuleContext modifier : modifiers) {
            if (!(modifier.getParent() instanceof ParserRuleContext list)) {
                throw new AssertionError("companionModifier has no modifier-list parent");
            }
            String listRule = ruleName(list, result.parser);
            if (!listRule.equals("functionModifierList") && !listRule.equals("propertyModifierList")) {
                throw new AssertionError("companionModifier belongs to unexpected " + listRule);
            }
            if (!(list.getParent() instanceof ParserRuleContext declaration)) {
                throw new AssertionError("companion modifier list has no declaration parent");
            }
            String declarationRule = ruleName(declaration, result.parser);
            if (listRule.equals("functionModifierList") && !declarationRule.equals("functionDeclaration")) {
                throw new AssertionError("Function companion modifier belongs to " + declarationRule);
            }
            if (listRule.equals("propertyModifierList") && !declarationRule.equals("propertyDeclaration")) {
                throw new AssertionError("Property companion modifier belongs to " + declarationRule);
            }
        }
    }

    private static void assertIdentifierUse(ParseResult result, String text, int expected) {
        int actual = 0;
        for (ParserRuleContext identifier : findRules(result.tree, result.parser, "simpleIdentifier")) {
            if (identifier.getText().equals(text)) {
                actual++;
            }
        }
        if (actual != expected) {
            throw new AssertionError("Expected " + expected + " simpleIdentifier uses of " + text + " but found " + actual);
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
            throw new AssertionError(label + " should produce a parser diagnostic");
        }
    }

    private static String printable(String value) {
        return value.replace("\r", "\\r").replace("\n", "\\n");
    }
}