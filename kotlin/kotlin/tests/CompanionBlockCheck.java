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

final class CompanionBlockCheck {
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
        ParseResult fixture = parseFile(Files.readString(Path.of("examples/kotlinFile/companion_blocks.kt")));
        assertNoErrors("companion block fixture", fixture.errors);
        assertRuleCount(fixture, "companionBlock", 6);
        assertRuleCount(fixture, "companionObject", 1);
        assertRuleCount(fixture, "companionModifier", 1);
        assertBlockOwnership(fixture);

        assertMemberCount("empty companion block", "class Host { companion {} }", 1);
        assertMemberCount("final companion block", "class Host { val before = 1\ncompanion { fun member() = 2 } }", 2);
        assertMemberCount("separated companion blocks", "class Host { companion {}\n; private companion {} }", 2);
        assertMemberCount("companion block in object", "object Host { companion { val value = 1 } }", 1);
        assertMemberCount("companion block in enum", "enum class Host { ENTRY; companion { fun member() = 1 } }", 1);

        for (String lineEnding : List.of("\r", "\n", "\r\n")) {
            ParseResult lineEndingFixture = parseFile(
                Files.readString(Path.of("examples/kotlinFile/companion_blocks.kt")).replace("\n", lineEnding));
            assertNoErrors("companion blocks with " + printable(lineEnding), lineEndingFixture.errors);
            assertRuleCount(lineEndingFixture, "companionBlock", 6);
            assertBlockOwnership(lineEndingFixture);
        }

        ParseResult script = parseScript("class ScriptHost { companion { fun member() = 1 } }");
        assertNoErrors("companion block in script class", script.errors);
        assertRuleCount(script, "companionBlock", 1);
        assertBlockOwnership(script);

        for (String source : List.of(
            "companion {}",
            "class Host { val before = 1; companion }",
            "class Host { val before = 1; companion Named {} }",
            "class Host { val before = 1; companion : Parent {} }",
            "class Host { companion { println(1) } }",
            "class Host { companion {} val value = 1 }")) {
            assertHasErrors("invalid companion block form " + source, parseFile(source).errors);
        }

        System.out.println("Companion block checks passed.");
    }

    private static void assertMemberCount(String label, String source, int expectedMembers) {
        ParseResult result = parseFile(source);
        assertNoErrors(label, result.errors);
        List<ParserRuleContext> memberContainers = new ArrayList<>();
        for (ParserRuleContext body : findRules(result.tree, result.parser, "classBody")) {
            if (body.getParent() instanceof ParserRuleContext parent) {
                String parentRule = ruleName(parent, result.parser);
                if (parentRule.equals("classDeclaration") || parentRule.equals("objectDeclaration")) {
                    memberContainers.add(body);
                }
            }
        }
        memberContainers.addAll(findRules(result.tree, result.parser, "enumClassBody"));
        if (memberContainers.size() != 1) {
            throw new AssertionError(label + ": expected one outer member container but found " + memberContainers.size());
        }
        int actual = directRuleChildren(memberContainers.get(0), result.parser, "classMemberDeclaration").size();
        if (actual != expectedMembers) {
            throw new AssertionError(label + ": expected " + expectedMembers + " members but found " + actual);
        }
    }

    private static void assertBlockOwnership(ParseResult result) {
        for (ParserRuleContext block : findRules(result.tree, result.parser, "companionBlock")) {
            if (!(block.getParent() instanceof ParserRuleContext member)
                || !ruleName(member, result.parser).equals("classMemberDeclaration")) {
                throw new AssertionError("companionBlock is not a direct classMemberDeclaration alternative");
            }
            List<ParserRuleContext> bodies = directRuleChildren(block, result.parser, "classBody");
            if (bodies.size() != 1) {
                throw new AssertionError("companionBlock should own exactly one classBody");
            }
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

    private static void assertRuleCount(ParseResult result, String rule, int expected) {
        int actual = findRules(result.tree, result.parser, rule).size();
        if (actual != expected) {
            throw new AssertionError("Expected " + expected + " " + rule + " nodes but found " + actual);
        }
    }

    private static List<ParserRuleContext> findRules(ParseTree tree, Parser parser, String rule) {
        List<ParserRuleContext> found = new ArrayList<>();
        collectRules(tree, parser, rule, found);
        return found;
    }

    private static void collectRules(ParseTree tree, Parser parser, String rule, List<ParserRuleContext> found) {
        if (tree instanceof ParserRuleContext context && ruleName(context, parser).equals(rule)) {
            found.add(context);
        }
        for (int i = 0; i < tree.getChildCount(); i++) {
            collectRules(tree.getChild(i), parser, rule, found);
        }
    }

    private static List<ParserRuleContext> directRuleChildren(ParserRuleContext context, Parser parser, String rule) {
        List<ParserRuleContext> found = new ArrayList<>();
        for (int i = 0; i < context.getChildCount(); i++) {
            if (context.getChild(i) instanceof ParserRuleContext child && ruleName(child, parser).equals(rule)) {
                found.add(child);
            }
        }
        return found;
    }

    private static String ruleName(ParserRuleContext context, Parser parser) {
        return parser.getRuleNames()[context.getRuleIndex()];
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