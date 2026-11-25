package org.antlr.grammars;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.atn.DecisionInfo;
import org.antlr.v4.runtime.tree.ParseTree;

final class PredictionPerformanceCheck {
    private static final class Metrics {
        private long sllDfaTransitions;
        private long sllAtnTransitions;
        private long llAtnTransitions;
        private long llFallbacks;
        private long ambiguities;
        private long predictionNanos;
        private final Map<String, Long> ruleTransitions = new HashMap<>();
        private final Map<String, Long> ruleFallbacks = new HashMap<>();

        private void add(Metrics other) {
            sllDfaTransitions += other.sllDfaTransitions;
            sllAtnTransitions += other.sllAtnTransitions;
            llAtnTransitions += other.llAtnTransitions;
            llFallbacks += other.llFallbacks;
            ambiguities += other.ambiguities;
            predictionNanos += other.predictionNanos;
            other.ruleTransitions.forEach((rule, count) -> ruleTransitions.merge(rule, count, Long::sum));
            other.ruleFallbacks.forEach((rule, count) -> ruleFallbacks.merge(rule, count, Long::sum));
        }
    }

    private static final class ParseResult {
        private final KotlinParser parser;
        private final ParserRuleContext tree;
        private final List<String> errors;
        private final Metrics metrics;

        private ParseResult(KotlinParser parser, ParserRuleContext tree, List<String> errors, Metrics metrics) {
            this.parser = parser;
            this.tree = tree;
            this.errors = errors;
            this.metrics = metrics;
        }
    }

    public static void main(String[] args) throws IOException {
        ParseResult modifiers = profile("class modifiers", "abstract sealed data class A", KotlinParser::kotlinFile);
        assertNoAmbiguities("class modifier alternatives", modifiers.metrics);
        assertRuleCount("ordinary class modifiers", modifiers, "modifier", 3);

        ParseResult parenthesizedType = profile("nested parenthesized type",
            "typealias A = (((String)))?", KotlinParser::kotlinFile);
        assertNoAmbiguities("parenthesized type alternatives", parenthesizedType.metrics);
        assertRuleCount("nested parenthesized types", parenthesizedType, "parenthesizedType", 3);
        assertNoParenthesesInTypeReferences(parenthesizedType);

        profile("file annotation list", "@file:JvmName(\"A\")\n@file:Suppress(\"x\")\nclass A", KotlinParser::kotlinFile);
        profileCorpus();
        System.out.println("Prediction performance checks passed.");
    }

    private static ParseResult profile(String label, String source, Function<KotlinParser, ParserRuleContext> entryPoint) {
        ParseResult result = parse(source, entryPoint);
        assertNoErrors(label, result);
        report(label, result.metrics);
        return result;
    }

    private static void profileCorpus() throws IOException {
        Metrics metrics = new Metrics();
        int checked = profileDirectory(Path.of("examples/kotlinFile"), "*.kt", KotlinParser::kotlinFile, metrics);
        checked += profileDirectory(Path.of("examples/script"), "*.kts", KotlinParser::script, metrics);
        if (checked == 0) {
            throw new AssertionError("No positive Kotlin fixtures were profiled");
        }
        if (metrics.llFallbacks > 1100) {
            throw new AssertionError("Positive corpus exceeded the LL fallback budget: " + metrics.llFallbacks);
        }
        System.out.println("positive fixtures=" + checked);
        report("positive corpus", metrics);
    }

    private static int profileDirectory(Path directory, String glob,
        Function<KotlinParser, ParserRuleContext> entryPoint, Metrics total)
        throws IOException {
        int checked = 0;
        try (DirectoryStream<Path> paths = Files.newDirectoryStream(directory, glob)) {
            for (Path path : paths) {
                if (Files.exists(Path.of(path + ".errors"))) {
                    continue;
                }
                ParseResult result = parse(Files.readString(path), entryPoint);
                assertNoErrors(path.toString(), result);
                total.add(result.metrics);
                checked++;
            }
        }
        return checked;
    }

    private static ParseResult parse(String source, Function<KotlinParser, ParserRuleContext> entryPoint) {
        List<String> errors = new ArrayList<>();
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        tokens.fill();

        KotlinParser parser = new KotlinParser(tokens);
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        parser.getInterpreter().clearDFA();
        parser.setProfile(true);
        ParserRuleContext tree = entryPoint.apply(parser);
        if (parser.getCurrentToken().getType() != Token.EOF) {
            errors.add("Unconsumed input " + parser.getCurrentToken().getText());
        }
        return new ParseResult(parser, tree, errors, collectMetrics(parser));
    }

    private static Metrics collectMetrics(KotlinParser parser) {
        Metrics result = new Metrics();
        DecisionInfo[] decisions = parser.getParseInfo().getDecisionInfo();
        for (int i = 0; i < decisions.length; i++) {
            DecisionInfo info = decisions[i];
            result.sllDfaTransitions += info.SLL_DFATransitions;
            result.sllAtnTransitions += info.SLL_ATNTransitions;
            result.llAtnTransitions += info.LL_ATNTransitions;
            result.llFallbacks += info.LL_Fallback;
            result.ambiguities += info.ambiguities.size();
            result.predictionNanos += info.timeInPrediction;
            String rule = parser.getRuleNames()[parser.getATN().getDecisionState(i).ruleIndex];
            result.ruleTransitions.merge(rule, info.SLL_ATNTransitions + info.LL_ATNTransitions, Long::sum);
            result.ruleFallbacks.merge(rule, (long) info.LL_Fallback, Long::sum);
        }
        return result;
    }

    private static void report(String label, Metrics metrics) {
        System.out.println(label + ": SLL-DFA=" + metrics.sllDfaTransitions
            + " SLL-ATN=" + metrics.sllAtnTransitions
            + " LL-ATN=" + metrics.llAtnTransitions
            + " LL-fallbacks=" + metrics.llFallbacks
            + " ambiguities=" + metrics.ambiguities
            + " prediction-ns=" + metrics.predictionNanos);
        for (String rule : List.of("classDeclarationModifierList", "classDeclarationModifier", "type", "typeReference",
            "parenthesizedType", "fileAnnotations", "fileAnnotation")) {
            long transitions = metrics.ruleTransitions.getOrDefault(rule, 0L);
            long fallbacks = metrics.ruleFallbacks.getOrDefault(rule, 0L);
            if (transitions != 0 || fallbacks != 0) {
                System.out.println("  " + rule + ": ATN=" + transitions + " fallbacks=" + fallbacks);
            }
        }
    }

    private static void assertNoAmbiguities(String label, Metrics metrics) {
        if (metrics.llFallbacks != 0 || metrics.ambiguities != 0) {
            throw new AssertionError(label + " still requires full-context prediction: fallbacks="
                + metrics.llFallbacks + ", ambiguities=" + metrics.ambiguities);
        }
    }

    private static void assertRuleCount(String label, ParseResult result, String rule, int expected) {
        int actual = countRules(result.tree, result.parser, rule);
        if (actual != expected) {
            throw new AssertionError(label + " expected " + expected + " " + rule + " nodes but found " + actual);
        }
    }

    private static int countRules(ParseTree tree, KotlinParser parser, String rule) {
        int count = tree instanceof ParserRuleContext context
            && parser.getRuleNames()[context.getRuleIndex()].equals(rule) ? 1 : 0;
        for (int i = 0; i < tree.getChildCount(); i++) {
            count += countRules(tree.getChild(i), parser, rule);
        }
        return count;
    }

    private static void assertNoParenthesesInTypeReferences(ParseResult result) {
        assertNoParenthesesInTypeReferences(result.tree, result.parser);
    }

    private static void assertNoParenthesesInTypeReferences(ParseTree tree, KotlinParser parser) {
        if (tree instanceof ParserRuleContext context
            && parser.getRuleNames()[context.getRuleIndex()].equals("typeReference")) {
            for (int i = 0; i < context.getChildCount(); i++) {
                if (context.getChild(i) instanceof org.antlr.v4.runtime.tree.TerminalNode terminal
                    && terminal.getSymbol().getType() == KotlinParser.LPAREN) {
                    throw new AssertionError("Parenthesized type was routed through typeReference: " + context.getText());
                }
            }
        }
        for (int i = 0; i < tree.getChildCount(); i++) {
            assertNoParenthesesInTypeReferences(tree.getChild(i), parser);
        }
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

    private static void assertNoErrors(String label, ParseResult result) {
        if (!result.errors.isEmpty()) {
            throw new AssertionError(label + " produced errors: " + result.errors);
        }
    }
}
