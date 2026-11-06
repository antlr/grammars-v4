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

final class AnnotationProjectionCheck {
    private static final String[] TARGETS = {
        "field", "file", "property", "get", "set", "receiver", "param", "setparam", "delegate"
    };

    private static final int[] TARGET_TYPES = {
        KotlinLexer.FIELD_SITE, KotlinLexer.FILE_SITE, KotlinLexer.PROPERTY_SITE, KotlinLexer.GET_SITE,
        KotlinLexer.SET_SITE, KotlinLexer.RECEIVER_SITE, KotlinLexer.PARAM_SITE, KotlinLexer.SETPARAM_SITE,
        KotlinLexer.DELEGATE_SITE
    };

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
        ParseResult fixture = parse(Files.readString(Path.of("examples/kotlinFile/annotation_projections.kt")), false);
        assertNoErrors("annotation projection fixture", fixture.errors);
        assertProjectionModifiers(fixture);
        assertCount(fixture, "annotationUseSiteTarget", 2);

        ParseResult crlf = parse(Files.readString(Path.of("examples/kotlinFile/annotation_projections.kt")).replace("\n", "\r\n"), false);
        assertNoErrors("CRLF annotation projection fixture", crlf.errors);
        assertProjectionModifiers(crlf);

        for (String target : TARGETS) {
            for (String suffix : List.of("", ".Qualified", "<Marker>", "(\"value\")")) {
                ParseResult ordinary = parse("@" + target + suffix + " class Annotated", false);
                assertNoErrors("ordinary annotation @" + target + suffix, ordinary.errors);
                assertCount(ordinary, "annotationName", 1);
                assertCount(ordinary, "annotationUseSiteTarget", 0);
            }

            if (!target.equals("file")) {
                ParseResult singleTarget = parse("@" + target + ": Marker class Targeted", false);
                assertNoErrors("single use-site target @" + target, singleTarget.errors);
                assertCount(singleTarget, "annotationUseSiteTarget", 1);

                ParseResult groupedTarget = parse("@" + target + "\n:\n[First Second] class Targeted", false);
                assertNoErrors("grouped use-site target @" + target, groupedTarget.errors);
                assertCount(groupedTarget, "annotationUseSiteTarget", 1);
            }
        }

        ParseResult ordinaryFile = parse("@file class FileAnnotationName", false);
        assertNoErrors("ordinary @file annotation", ordinaryFile.errors);
        assertCount(ordinaryFile, "fileAnnotation", 0);
        assertCount(ordinaryFile, "annotationName", 1);

        for (String source : List.of(
            "@file\n:\nJvmName(\"LfName\")\npackage sample\nclass FileAnnotated",
            "@file\r\n:\r\n[\r\nJvmName(\"CrLfName\")\r\nSuppress\r\n]\r\npackage sample\r\nclass FileAnnotated"
        )) {
            ParseResult fileAnnotation = parse(source, false);
            assertNoErrors("multiline file annotation " + source, fileAnnotation.errors);
            assertCount(fileAnnotation, "fileAnnotation", 1);
        }

        ParseResult script = parse("@delegate val scriptValue = 1\nprintln(scriptValue)", true);
        assertNoErrors("script ordinary annotation", script.errors);
        assertCount(script, "annotationName", 1);

        assertTargetTokens("@field @file @property @get @set @receiver @param @setparam @delegate");
        assertTargetTokens("(@field @file @property @get @set @receiver @param @setparam @delegate)");
        assertTargetTokens("\"${@field @file @property @get @set @receiver @param @setparam @delegate}\"");

        for (String source : List.of(
            "fun malformed(value: Box<@First in>) {}",
            "@get: class MissingAnnotation",
            "@file\n: package sample"
        )) {
            assertHasErrors("malformed annotation or projection " + source, parse(source, false).errors);
        }

        System.out.println("Annotation and projection checks passed.");
    }

    private static ParseResult parse(String source, boolean script) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        return new ParseResult(parser, script ? parser.script() : parser.kotlinFile(), errors);
    }

    private static void assertProjectionModifiers(ParseResult result) {
        List<ParserRuleContext> projections = findRules(result.tree, result.parser, "typeProjection");
        int checked = 0;
        for (ParserRuleContext projection : projections) {
            ParserRuleContext modifierList = directRuleChild(projection, result.parser, "typeProjectionModifierList");
            if (modifierList == null) {
                continue;
            }
            int annotations = directRuleChildren(modifierList, result.parser, "annotations").size();
            int variances = directRuleChildren(modifierList, result.parser, "varianceAnnotation").size();
            if (annotations == 0 || variances != 1) {
                throw new AssertionError("Expected annotated projection modifiers with one variance: " + modifierList.getText());
            }
            checked++;
        }
        if (checked != 4) {
            throw new AssertionError("Expected four annotated variance projections but found " + checked);
        }
    }

    private static void assertTargetTokens(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        lexer.removeErrorListeners();
        CommonTokenStream stream = new CommonTokenStream(lexer);
        stream.fill();
        List<Integer> actual = new ArrayList<>();
        for (Token token : stream.getTokens()) {
            if (token.getText().startsWith("@")) {
                for (int type : TARGET_TYPES) {
                    if (token.getType() == type) {
                        actual.add(token.getType());
                        break;
                    }
                }
            }
        }
        List<Integer> expected = new ArrayList<>();
        for (int type : TARGET_TYPES) {
            expected.add(type);
        }
        if (!actual.equals(expected)) {
            throw new AssertionError("Expected combined target tokens " + expected + " but found " + actual + " for " + source);
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

    private static ParserRuleContext directRuleChild(ParserRuleContext context, Parser parser, String rule) {
        List<ParserRuleContext> children = directRuleChildren(context, parser, rule);
        return children.isEmpty() ? null : children.get(0);
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
}
