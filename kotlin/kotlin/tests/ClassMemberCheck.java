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

final class ClassMemberCheck {
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
        ParseResult fixture = parse(Files.readString(Path.of("examples/kotlinFile/class_members.kt")));
        assertNoErrors("class member fixture", fixture.errors);
        assertCount("class member fixture", countRules(fixture.tree, fixture.parser, "classMemberDeclaration"), 18);

        assertBody("compact final property", "class Compact { val value = 1 }", 1);
        assertBody("compact final function", "class Compact { fun value() = 1 }", 1);
        assertBody("newline-separated members", "class Compact { val first = 1\nfun second() = 2 }", 2);
        assertBody("semicolon-separated members", "class Compact { val first = 1; fun second() = 2 }", 2);
        assertBody("mixed and trailing separators", "class Compact { val first = 1\n; fun second() = 2;\n}", 2);
        assertBody("CRLF-separated members", "class Compact { val first = 1\r\nfun second() = 2\r\n}", 2);

        ParseResult adjacentMembers = parse("class Invalid { val first = 1 val second = 2 }");
        assertHasErrors("members without a separator", adjacentMembers.errors);
        ParseResult adjacentCommentedMembers = parse("class Invalid { val first = 1 /* no separator */ val second = 2 }");
        assertHasErrors("members separated only by a comment", adjacentCommentedMembers.errors);
        System.out.println("Class member checks passed.");
    }

    private static void assertBody(String label, String source, int expectedMembers) {
        ParseResult result = parse(source);
        assertNoErrors(label, result.errors);
        List<ParserRuleContext> bodies = findRules(result.tree, result.parser, "classBody");
        if (bodies.size() != 1) {
            throw new AssertionError(label + ": expected one classBody but found " + bodies.size());
        }
        int actualMembers = directRuleChildren(bodies.get(0), result.parser, "classMemberDeclaration").size();
        if (actualMembers != expectedMembers) {
            throw new AssertionError(label + ": expected " + expectedMembers + " members but found " + actualMembers);
        }
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

    private static int countRules(ParseTree tree, Parser parser, String rule) {
        int count = 0;
        if (tree instanceof ParserRuleContext context && ruleName(context, parser).equals(rule)) {
            count++;
        }
        for (int i = 0; i < tree.getChildCount(); i++) {
            count += countRules(tree.getChild(i), parser, rule);
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

    private static void assertCount(String label, int actual, int expected) {
        if (actual != expected) {
            throw new AssertionError(label + ": expected " + expected + " members but found " + actual);
        }
    }
}
