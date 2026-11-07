package org.antlr.grammars;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.function.Consumer;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.atn.DecisionInfo;

final class ImportPerformanceCheck {
    private static final class ParseResult {
        private final List<String> errors;
        private final int importHeaders;
        private final long fallbacks;
        private final long predictionNanos;
        private final long atnTransitions;
        private final long sllLookahead;
        private final long llLookahead;

        private ParseResult(List<String> errors, int importHeaders, long fallbacks, long predictionNanos,
            long atnTransitions, long sllLookahead, long llLookahead) {
            this.errors = errors;
            this.importHeaders = importHeaders;
            this.fallbacks = fallbacks;
            this.predictionNanos = predictionNanos;
            this.atnTransitions = atnTransitions;
            this.sllLookahead = sllLookahead;
            this.llLookahead = llLookahead;
        }
    }

    public static void main(String[] args) {
        checkImportSyntax();
        checkImportPerformance(args.length == 0 ? 100 : Integer.parseInt(args[0]));
        System.out.println("Import checks passed.");
    }

    private static void checkImportSyntax() {
        assertParses("ordinary import", "import sample.name", KotlinParser::kotlinFile);
        assertParses("wildcard import", "import sample.name.*", KotlinParser::kotlinFile);
        assertParses("aliased import", "import sample.name as alias", KotlinParser::kotlinFile);
        assertParses("escaped import", "import sample.`when` as `class`", KotlinParser::kotlinFile);
        assertParses("soft-keyword import", "import sample.context.import.value", KotlinParser::kotlinFile);
        assertParses("newline-separated imports", "import sample.name\nimport sample.other.*\nclass Example", KotlinParser::kotlinFile);
        assertParses("semicolon-separated imports", "import sample.name; import sample.other.*; class Example", KotlinParser::kotlinFile);
        assertParses("script import", "import sample.name\nval value = 1", KotlinParser::script);
        assertRejects("incomplete import", "import sample.", KotlinParser::kotlinFile);
        assertRejects("missing import separator", "import sample.name val value = 1", KotlinParser::kotlinFile);
    }

    private static void checkImportPerformance(int count) {
        String source = "import sample.name\n".repeat(count) + "fun test() {\n" + "val value = 1\n".repeat(100) + "}\n";
        long start = System.nanoTime();
        ParseResult result = parseSource(source, KotlinParser::kotlinFile, true);
        double parseMillis = (System.nanoTime() - start) / 1e6;
        assertNoErrors("repeated imports", result);
        assertTrue("each import should be parsed", result.importHeaders == count,
            "expected " + count + " import headers but observed " + result.importHeaders);
        assertTrue("importHeader should not require full-context fallback", result.fallbacks == 0,
            "observed " + result.fallbacks + " fallbacks");
        System.out.printf(Locale.ROOT,
            "imports=%d parsedHeaders=%d parse=%.3f ms importHeader fallback=%d prediction=%.3f ms SLL-look=%d LL-look=%d ATN=%d%n",
            count, result.importHeaders, parseMillis, result.fallbacks, result.predictionNanos / 1e6,
            result.sllLookahead, result.llLookahead, result.atnTransitions);
    }

    private static ParseResult parseSource(String source, Consumer<KotlinParser> entryPoint, boolean profile) {
        List<String> errors = new ArrayList<>();
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        int[] importHeaders = {0};
        if (profile) {
            parser.addParseListener(new KotlinParserBaseListener() {
                @Override
                public void enterEveryRule(org.antlr.v4.runtime.ParserRuleContext context) {
                    if (context.getRuleIndex() == KotlinParser.RULE_importHeader) {
                        importHeaders[0]++;
                    }
                }
            });
        }
        parser.setProfile(profile);
        entryPoint.accept(parser);
        if (parser.getCurrentToken().getType() != Token.EOF) {
            errors.add("Unconsumed input " + parser.getCurrentToken().getText());
        }
        long fallbacks = 0;
        long predictionNanos = 0;
        long atnTransitions = 0;
        long sllLookahead = 0;
        long llLookahead = 0;
        if (profile) {
            for (DecisionInfo info : parser.getParseInfo().getDecisionInfo()) {
                String rule = parser.getRuleNames()[parser.getATN().decisionToState.get(info.decision).ruleIndex];
                if (rule.equals("importHeader")) {
                    fallbacks += info.LL_Fallback;
                    predictionNanos += info.timeInPrediction;
                    atnTransitions += info.SLL_ATNTransitions + info.LL_ATNTransitions;
                    sllLookahead += info.SLL_TotalLook;
                    llLookahead += info.LL_TotalLook;
                }
            }
        }
        return new ParseResult(errors, importHeaders[0], fallbacks, predictionNanos, atnTransitions, sllLookahead, llLookahead);
    }

    private static void assertParses(String label, String source, Consumer<KotlinParser> entryPoint) {
        assertNoErrors(label, parseSource(source, entryPoint, false));
    }

    private static void assertRejects(String label, String source, Consumer<KotlinParser> entryPoint) {
        List<String> errors = parseSource(source, entryPoint, false).errors;
        if (errors.isEmpty()) {
            throw new AssertionError(label + " should produce a parse error");
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

    private static void assertTrue(String label, boolean condition, String detail) {
        if (!condition) {
            throw new AssertionError(label + ": " + detail);
        }
    }
}
