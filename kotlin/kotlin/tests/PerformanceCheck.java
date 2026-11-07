package org.antlr.grammars;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.atn.DecisionInfo;
import org.antlr.v4.runtime.atn.LexerATNSimulator;

final class PerformanceCheck {
    private static final class CountingLexer extends KotlinLexer {
        private final CountingLexerATNSimulator simulator;

        private CountingLexer(String source) {
            super(CharStreams.fromString(source));
            LexerATNSimulator interpreter = getInterpreter();
            simulator = new CountingLexerATNSimulator(this, interpreter);
            setInterpreter(simulator);
        }
    }

    private static final class CountingLexerATNSimulator extends LexerATNSimulator {
        private int matchCount;

        private CountingLexerATNSimulator(KotlinLexer lexer, LexerATNSimulator interpreter) {
            super(lexer, interpreter.atn, interpreter.decisionToDFA, interpreter.getSharedContextCache());
        }

        @Override
        public int match(org.antlr.v4.runtime.CharStream input, int mode) {
            matchCount++;
            return super.match(input, mode);
        }
    }

    private static final class ParseResult {
        private final List<String> errors;
        private final long predictionTransitions;
        private final long postfixCallChainTransitions;
        private final int postfixCallChainFallbacks;

        private ParseResult(List<String> errors, long predictionTransitions, long postfixCallChainTransitions,
            int postfixCallChainFallbacks) {
            this.errors = errors;
            this.predictionTransitions = predictionTransitions;
            this.postfixCallChainTransitions = postfixCallChainTransitions;
            this.postfixCallChainFallbacks = postfixCallChainFallbacks;
        }
    }

    public static void main(String[] args) throws IOException {
        checkPostfixChains();
        checkTriviaBoundaries();
        checkExampleCorpus();
        checkWhitespaceMatching();
        System.out.println("Performance checks passed.");
    }

    private static void checkExampleCorpus() throws IOException {
        int checked = checkExamples(Path.of("examples/kotlinFile"), "*.kt", KotlinParser::kotlinFile);
        checked += checkExamples(Path.of("examples/script"), "*.kts", KotlinParser::script);
        System.out.println("Positive examples parsed: " + checked);
    }

    private static int checkExamples(Path directory, String glob, Consumer<KotlinParser> entryPoint) throws IOException {
        int checked = 0;
        try (DirectoryStream<Path> paths = Files.newDirectoryStream(directory, glob)) {
            for (Path path : paths) {
                if (Files.exists(Path.of(path + ".errors"))) {
                    continue;
                }
                ParseResult result = parseSource(Files.readString(path), entryPoint);
                assertNoErrors(path.toString(), result);
                checked++;
            }
        }
        return checked;
    }

    private static void checkWhitespaceMatching() {
        String whitespace = " \t\f".repeat(4096);
        CountingLexer lexer = new CountingLexer(whitespace + "value");
        lexer.removeErrorListeners();
        lexer.getAllTokens();
        int matchCount = lexer.simulator.matchCount;
        System.out.println("Whitespace lexer match attempts: " + matchCount);
        assertTrue("horizontal whitespace should be matched as a run", matchCount <= 4,
            "expected at most 4 lexer matches but observed " + matchCount);
    }

    private static void checkTriviaBoundaries() {
        String invalidShebang = " \t\f#! script\nval value = 1";
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(invalidShebang));
        List<? extends Token> tokens = lexer.getAllTokens();
        assertTrue("leading trivia should remain part of invalid shebang", !tokens.isEmpty() &&
            "InvalidShebangLine".equals(lexer.getVocabulary().getSymbolicName(tokens.get(0).getType())), tokens.toString());

        assertNoErrors("whitespace in nested lexer modes", parseExpression("\"${call( \t\f arg )}\""));
    }

    private static void checkPostfixChains() {
        ParseResult operations = parseExpression("value++++");
        assertNoErrors("postfix operations without calls", operations);

        ParseResult callsAndOperations = parseExpression("factory()++()--()++");
        assertNoErrors("postfix call and operation chain", callsAndOperations);
        reportPostfixMetrics("calls and operations", callsAndOperations);

        String repeatedCalls = "factory()" + "()".repeat(10);
        assertNoErrors("repeated argument calls", reportPostfixMetrics("repeated argument calls",
            parseExpression(repeatedCalls)));
        assertNoErrors("generic call chain", reportPostfixMetrics("generic call chain",
            parseExpression("factory<First>()<Second>()<Third>()")));
        assertNoErrors("lambda-first call chain", reportPostfixMetrics("lambda-first call chain",
            parseExpression("factory {}" + "()".repeat(8))));
        assertNoErrors("lambda-middle call chain", reportPostfixMetrics("lambda-middle call chain",
            parseExpression("factory()" + "()".repeat(4) + " {} " + "()".repeat(4))));
        assertNoErrors("lambda-last call chain", reportPostfixMetrics("lambda-last call chain",
            parseExpression("factory()" + "()".repeat(8) + " {}")));

        assertHasErrors("incomplete postfix call chain", parseExpression("factory()++("));
        assertHasErrors("multiple adjacent trailing lambdas", parseExpression("factory() {} {}"));
    }

    private static ParseResult reportPostfixMetrics(String label, ParseResult result) {
        System.out.println(label + " postfixCallChain transitions: " + result.postfixCallChainTransitions
            + ", LL fallbacks: " + result.postfixCallChainFallbacks
            + ", total parser transitions: " + result.predictionTransitions);
        return result;
    }

    private static ParseResult parseExpression(String source) {
        return parseSource(source, KotlinParser::expression);
    }

    private static ParseResult parseSource(String source, Consumer<KotlinParser> entryPoint) {
        List<String> errors = new ArrayList<>();
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        parser.setProfile(true);
        entryPoint.accept(parser);
        if (parser.getCurrentToken().getType() != Token.EOF) {
            errors.add("Unconsumed input " + parser.getCurrentToken().getText());
        }
        long transitions = 0;
        long postfixCallChainTransitions = 0;
        int postfixCallChainFallbacks = 0;
        DecisionInfo[] decisionInfo = parser.getParseInfo().getDecisionInfo();
        for (int i = 0; i < decisionInfo.length; i++) {
            DecisionInfo info = decisionInfo[i];
            transitions += info.SLL_ATNTransitions + info.LL_ATNTransitions;
            if (parser.getRuleNames()[parser.getATN().getDecisionState(i).ruleIndex].equals("postfixCallChain")) {
                postfixCallChainTransitions += info.SLL_ATNTransitions + info.LL_ATNTransitions;
                postfixCallChainFallbacks += info.LL_Fallback;
            }
        }
        return new ParseResult(errors, transitions, postfixCallChainTransitions, postfixCallChainFallbacks);
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

    private static void assertNoErrors(String label, ParseResult result) {
        if (!result.errors.isEmpty()) {
            throw new AssertionError(label + " produced errors: " + result.errors);
        }
    }

    private static void assertHasErrors(String label, ParseResult result) {
        if (result.errors.isEmpty()) {
            throw new AssertionError(label + " should produce a parse error");
        }
    }

    private static void assertTrue(String label, boolean condition, String detail) {
        if (!condition) {
            throw new AssertionError(label + ": " + detail);
        }
    }
}