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
import org.antlr.v4.runtime.tree.TerminalNode;

final class TypeOperationCheck {
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
        ParseResult fixture = parse(Files.readString(Path.of("examples/kotlinFile/type_operations.kt")));
        assertNoErrors("type operation fixture", fixture.errors);
        assertCastTargetsAreTypes(fixture, 10);
        assertOperationSequence(fixture, List.of("in", "is", "!in", "!is"),
            List.of("infixExpression", "type", "infixExpression", "type"));
        assertCount("cast operations", fixture, "typeOperation", 10);

        for (String target : List.of("1", "String()", "-String")) {
            ParseResult invalid = parse("fun invalid(value: Any) { val result = value as " + target + " }");
            assertHasErrors("invalid cast target " + target, invalid.errors);
        }

        ParseResult precedence = parse("fun precedence(value: Any, other: Int) { val result = value as List<String> * other + 1 }");
        assertNoErrors("cast precedence", precedence.errors);
        assertCastTargetsAreTypes(precedence, 1);
        assertCastPrecedence(precedence);
        System.out.println("Type operation checks passed.");
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

    private static void assertCastTargetsAreTypes(ParseResult result, int expectedCasts) {
        int casts = 0;
        for (ParserRuleContext context : findRules(result.tree, result.parser, "infixExpression")) {
            List<ParserRuleContext> operations = directRuleChildren(context, result.parser, "typeOperation");
            if (operations.isEmpty()) {
                continue;
            }
            List<ParserRuleContext> types = directRuleChildren(context, result.parser, "type");
            if (operations.size() != types.size()) {
                throw new AssertionError("Each cast operation must have a type target: " + context.getText());
            }
            casts += operations.size();
        }
        if (casts != expectedCasts) {
            throw new AssertionError("Expected " + expectedCasts + " cast operations but found " + casts);
        }
    }

    private static void assertOperationSequence(ParseResult result, List<String> operators, List<String> operands) {
        for (ParserRuleContext context : findRules(result.tree, result.parser, "infixExpression")) {
            List<String> actualOperators = new ArrayList<>();
            List<String> actualOperands = new ArrayList<>();
            collectOperations(context, result.parser, actualOperators, actualOperands);
            if (actualOperators.equals(operators)) {
                if (!actualOperands.equals(operands)) {
                    throw new AssertionError("Expected operands " + operands + " but found " + actualOperands);
                }
                return;
            }
        }
        throw new AssertionError("Did not find infixExpression chain with operators " + operators);
    }

    private static void assertCastPrecedence(ParseResult result) {
        for (ParserRuleContext addition : findRules(result.tree, result.parser, "infixExpression")) {
            if (directRuleChildren(addition, result.parser, "additiveOperator").isEmpty()) {
                continue;
            }
            for (ParserRuleContext multiplication : directRuleChildren(addition, result.parser, "infixExpression")) {
                if (directRuleChildren(multiplication, result.parser, "multiplicativeOperation").isEmpty()) {
                    continue;
                }
                for (ParserRuleContext cast : directRuleChildren(multiplication, result.parser, "infixExpression")) {
                    if (!directRuleChildren(cast, result.parser, "typeOperation").isEmpty()) {
                        return;
                    }
                }
            }
        }
        throw new AssertionError("Expected cast to bind before multiplication, and multiplication before addition");
    }

    private static void collectOperations(ParseTree tree, Parser parser, List<String> operators, List<String> operands) {
        for (int i = 0; i < tree.getChildCount(); i++) {
            ParseTree child = tree.getChild(i);
            if (!(child instanceof ParserRuleContext operation)) {
                continue;
            }
            String rule = ruleName(operation, parser);
            if (rule.equals("inOperator") || rule.equals("isOperator")) {
                operators.add(operation.getText().strip());
                int operandIndex = i + 1;
                while (operandIndex < tree.getChildCount() && tree.getChild(operandIndex) instanceof TerminalNode) {
                    operandIndex++;
                }
                if (operandIndex == tree.getChildCount()
                        || !(tree.getChild(operandIndex) instanceof ParserRuleContext operand)) {
                    throw new AssertionError("Missing operand after " + rule + " in " + tree.getText());
                }
                operands.add(ruleName(operand, parser));
            }
            collectOperations(child, parser, operators, operands);
        }
    }

    private static void assertCount(String label, ParseResult result, String rule, int expected) {
        int actual = findRules(result.tree, result.parser, rule).size();
        if (actual != expected) {
            throw new AssertionError(label + ": expected " + expected + " " + rule + " nodes but found " + actual);
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
