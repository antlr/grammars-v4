package org.antlr.grammars;

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
import org.antlr.v4.runtime.atn.DecisionInfo;
import org.antlr.v4.runtime.tree.ParseTree;

final class NavigationPerformanceCheck {
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

    private static final class Metrics {
        private long sllDfaTransitions;
        private long sllAtnTransitions;
        private long llAtnTransitions;
        private long llFallbacks;
        private long predictionNanos;
        private long navigationAtnTransitions;
        private long navigationFallbacks;
        private long postfixExpressionAtnTransitions;
        private long postfixExpressionFallbacks;
    }

    public static void main(String[] args) {
        checkNavigationProfiles();
        checkNavigationSyntax();
        checkFlatSelectorOwnership();
        System.out.println("Navigation performance checks passed.");
    }

    private static void checkNavigationProfiles() {
        for (int length : List.of(2, 4, 8, 16)) {
            String source = navigationChain(length, false);
            ParseResult cold = parseExpression(source, true, true);
            assertNoErrors("cold navigation chain of length " + length, cold);
            report("cold", length, cold.metrics);

            for (int i = 0; i < 5; i++) {
                assertNoErrors("warmup navigation chain of length " + length, parseExpression(source, false));
            }
            ParseResult warm = parseExpression(source, true);
            assertNoErrors("warm navigation chain of length " + length, warm);
            assertTrue("warm navigation prediction work", warm.metrics.llAtnTransitions <= 2L * length + 4,
                "LL ATN transitions grew beyond the linear selector budget: " + warm.metrics.llAtnTransitions);
            report("warm", length, warm.metrics);
        }

        String callChain = navigationChain(8, true);
        ParseResult coldCalls = parseExpression(callChain, true, true);
        assertNoErrors("cold call/navigation chain", coldCalls);
        report("cold calls", 8, coldCalls.metrics);
        for (int i = 0; i < 5; i++) {
            assertNoErrors("warmup call/navigation chain", parseExpression(callChain, false));
        }
        ParseResult warmCalls = parseExpression(callChain, true);
        assertNoErrors("warm call/navigation chain", warmCalls);
        assertTrue("warm call/navigation prediction work", warmCalls.metrics.llAtnTransitions <= 4L * 8 + 4,
            "LL ATN transitions grew beyond the linear call/navigation budget: " + warmCalls.metrics.llAtnTransitions);
        report("warm calls", 8, warmCalls.metrics);
    }

    private static String navigationChain(int length, boolean calls) {
        StringBuilder source = new StringBuilder("root");
        for (int i = 0; i < length; i++) {
            source.append(".member");
            if (calls) {
                source.append("()");
            }
        }
        return source.toString();
    }

    private static void report(String cacheState, int length, Metrics metrics) {
        System.out.println(cacheState + " selectors=" + length
            + " SLL-DFA=" + metrics.sllDfaTransitions
            + " SLL-ATN=" + metrics.sllAtnTransitions
            + " LL-ATN=" + metrics.llAtnTransitions
            + " LL-fallbacks=" + metrics.llFallbacks
            + " prediction-ns=" + metrics.predictionNanos
            + " navigation-ATN=" + metrics.navigationAtnTransitions
            + " navigation-fallbacks=" + metrics.navigationFallbacks
            + " postfix-expression-ATN=" + metrics.postfixExpressionAtnTransitions
            + " postfix-expression-fallbacks=" + metrics.postfixExpressionFallbacks);
    }

    private static void checkNavigationSyntax() {
        for (String expression : List.of(
                 "root.branch",
                 "factory().branch()",
                 "root?.branch?.next()",
                 "root.(branch + value)",
                 "root.items[0].value()",
                 "factory<First, Second>().branch()",
                 "root.inline",
                 "root.`when`",
                 "root\n.branch()",
                 "root.class",
                 "factory()::run.invoke()",
                 "factory() {}.next()")) {
            assertNoErrors("navigation syntax " + expression, parseExpression(expression, false));
        }

        for (String expression : List.of("root.", "root.1", "root.+value", "root.()")) {
            assertHasErrors("invalid navigation syntax " + expression, parseExpression(expression, false));
        }
    }

    private static void checkFlatSelectorOwnership() {
        String source = "root.branch().leaf()";
        ParseResult result = parseExpression(source, false);
        assertNoErrors("flat navigation ownership", result);
        ParserRuleContext chain = findRuleWithText(result.tree, result.parser, "postfixUnaryExpression", source);
        if (chain == null) {
            throw new AssertionError("Did not find the full postfix expression for " + source);
        }

        List<ParserRuleContext> operations = directRuleChildren(chain, result.parser, "postfixUnaryOperation");
        List<ParserRuleContext> navigation = new ArrayList<>();
        for (ParserRuleContext operation : operations) {
            if (!directRuleChildren(operation, result.parser, "memberAccessOperator").isEmpty()) {
                navigation.add(operation);
            }
        }
        assertTrue("flat navigation ownership", navigation.size() == 2,
            "expected two direct navigation operations but found " + navigation.size() + ": " + chain.getText());
        for (ParserRuleContext operation : navigation) {
            assertTrue("flat navigation selector", directRuleChildren(operation, result.parser, "postfixUnaryExpression").isEmpty(),
                "selector recursively owns a postfix expression: " + operation.getText());
            assertTrue("flat navigation selector", directRuleChildren(operation, result.parser, "simpleIdentifier").size() == 1,
                "expected one direct simpleIdentifier selector: " + operation.getText());
        }
        assertTrue("flat call ownership", directRuleChildren(chain, result.parser, "postfixCallChain").size() == 2,
            "expected calls after each selector to belong to the outer postfix expression: " + chain.getText());

        assertDirectSelector("root.branch", "simpleIdentifier");
        assertDirectSelector("root.(branch)", "parenthesizedExpression");
        assertDirectSelector("root.class", "CLASS");
    }

    private static void assertDirectSelector(String source, String selectorRule) {
        ParseResult result = parseExpression(source, false);
        assertNoErrors("selector form " + source, result);
        ParserRuleContext chain = findRuleWithText(result.tree, result.parser, "postfixUnaryExpression", source);
        if (chain == null) {
            throw new AssertionError("Did not find the full postfix expression for " + source);
        }
        List<ParserRuleContext> operations = directRuleChildren(chain, result.parser, "postfixUnaryOperation");
        ParserRuleContext navigation = null;
        for (ParserRuleContext operation : operations) {
            if (!directRuleChildren(operation, result.parser, "memberAccessOperator").isEmpty()) {
                navigation = operation;
                break;
            }
        }
        if (navigation == null || !directRuleChildren(navigation, result.parser, "postfixUnaryExpression").isEmpty()) {
            throw new AssertionError("Expected a flat navigation selector for " + source);
        }
        boolean hasSelector = "CLASS".equals(selectorRule)
            ? hasDirectToken(navigation, KotlinParser.CLASS)
            : directRuleChildren(navigation, result.parser, selectorRule).size() == 1;
        assertTrue("selector form " + source, hasSelector,
            "expected direct " + selectorRule + " selector but found " + navigation.getText());
    }

    private static ParseResult parseExpression(String source, boolean profile) {
        return parseExpression(source, profile, false);
    }

    private static ParseResult parseExpression(String source, boolean profile, boolean clearDfa) {
        List<String> errors = new ArrayList<>();
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        tokens.fill();
        KotlinParser parser = new KotlinParser(tokens);
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        if (profile) {
            parser.setProfile(true);
        }
        if (clearDfa) {
            parser.getInterpreter().clearDFA();
        }
        ParserRuleContext tree = parser.expression();
        if (parser.getCurrentToken().getType() != Token.EOF) {
            errors.add("Unconsumed input " + parser.getCurrentToken().getText());
        }
        Metrics metrics = profile ? collectMetrics(parser) : new Metrics();
        return new ParseResult(parser, tree, errors, metrics);
    }

    private static Metrics collectMetrics(KotlinParser parser) {
        Metrics metrics = new Metrics();
        DecisionInfo[] decisions = parser.getParseInfo().getDecisionInfo();
        for (int i = 0; i < decisions.length; i++) {
            DecisionInfo info = decisions[i];
            metrics.sllDfaTransitions += info.SLL_DFATransitions;
            metrics.sllAtnTransitions += info.SLL_ATNTransitions;
            metrics.llAtnTransitions += info.LL_ATNTransitions;
            metrics.llFallbacks += info.LL_Fallback;
            metrics.predictionNanos += info.timeInPrediction;
            if (parser.getRuleNames()[parser.getATN().getDecisionState(i).ruleIndex].equals("postfixUnaryOperation")) {
                metrics.navigationAtnTransitions += info.SLL_ATNTransitions + info.LL_ATNTransitions;
                metrics.navigationFallbacks += info.LL_Fallback;
            }
            if (parser.getRuleNames()[parser.getATN().getDecisionState(i).ruleIndex].equals("postfixUnaryExpression")) {
                metrics.postfixExpressionAtnTransitions += info.SLL_ATNTransitions + info.LL_ATNTransitions;
                metrics.postfixExpressionFallbacks += info.LL_Fallback;
            }
        }
        return metrics;
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

    private static ParserRuleContext findRuleWithText(ParseTree tree, Parser parser, String rule, String text) {
        if (tree instanceof ParserRuleContext context && ruleName(context, parser).equals(rule)
            && context.getText().equals(text)) {
            return context;
        }
        for (int i = 0; i < tree.getChildCount(); i++) {
            ParserRuleContext found = findRuleWithText(tree.getChild(i), parser, rule, text);
            if (found != null) {
                return found;
            }
        }
        return null;
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

    private static boolean hasDirectToken(ParserRuleContext context, int tokenType) {
        for (int i = 0; i < context.getChildCount(); i++) {
            if (context.getChild(i) instanceof org.antlr.v4.runtime.tree.TerminalNode terminal
                && terminal.getSymbol().getType() == tokenType) {
                return true;
            }
        }
        return false;
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
