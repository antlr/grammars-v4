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
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

final class ExpressionHierarchyCheck {
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

    public static void main(String[] args) {
        ParseResult precedence = parseExpression("a || b && c == d < e + f * g");
        assertNoErrors("combined precedence", precedence);
        assertShape(precedence, "(|| a (&& b (== c (< d (+ e (* f g))))))");
        assertCount(precedence, "comparison", 1);

        ParseResult ordinary = parseExpression("a + b * c");
        assertNoErrors("ordinary infix expression", ordinary);
        assertShape(ordinary, "(+ a (* b c))");
        assertCount(ordinary, "comparison", 0);
        assertDirectParent(ordinary, "infixExpression", "expression");

        ParseResult comparison = parseExpression("a < b");
        assertNoErrors("comparison expression", comparison);
        assertShape(comparison, "(< a b)");
        assertCount(comparison, "comparison", 1);
        assertDirectParent(comparison, "comparison", "expression");

        ParseResult associativity = parseExpression("a - b - c");
        assertNoErrors("left-associative subtraction", associativity);
        assertShape(associativity, "(- (- a b) c)");
        assertShape(parseExpression("a == b == c"), "(== (== a b) c)");
        assertShape(parseExpression("a && b && c"), "(&& (&& a b) c)");
        assertShape(parseExpression("a || b || c"), "(|| (|| a b) c)");
        assertShape(parseExpression("a ?: b ?: c"), "(?: (?: a b) c)");
        assertShape(parseExpression("a + b .. c * d"), "(.. (+ a b) (* c d))");
        assertShape(parseExpression("a foo b + c"), "(foo a (+ b c))");
        assertShape(parseExpression("a foo b foo c"), "(foo (foo a b) c)");
        assertShape(parseExpression("a foo b ?: c"), "(?: (foo a b) c)");
        assertShape(parseExpression("a ?: b foo c"), "(?: a (foo b c))");
        assertShape(parseExpression("a in b + c is T !in d !is U"),
            "(!is (!in (is (in a (+ b c)) T) d) U)");
        assertShape(parseExpression("a is T in b !is U !in c"),
            "(!in (!is (in (is a T) b) U) c)");

        ParseResult casts = parseExpression("-a as T * b + c");
        assertNoErrors("cast precedence", casts);
        assertShape(casts, "(+ (* (as -a T) b) c)");
        assertCount(casts, "typeOperation", 1);
        assertDirectChildCount(casts, "infixExpression", "type", 1);

        assertNoErrors("parenthesized precedence", parseExpression("(a + b) * c"));
        assertNoErrors("postfix call chain", parseExpression("factory() { first(); second() }"));
        assertNoErrors("newline after binary operator", parseExpression("a +\n b"));
        assertNoErrors("newline after prefix operator", parseExpression("-\na"));
        assertNoErrors("newline after navigation operator", parseExpression("a.\nb"));
        assertHasErrors("newline before binary operator", parseExpression("a\n + b"));

        assertHasErrors("comparison chain", parseExpression("a < b < c"));
        assertHasErrors("assignment as expression", parseExpression("a = b"));
        assertHasErrors("loop as expression", parseExpression("while (ready) run()"));
        assertHasErrors("multiple trailing lambdas", parseExpression("factory {} {}"));

        ParseResult reference = parseExpression("::member");
        assertNoErrors("callable reference", reference);
        assertDirectParent(reference, "callableReference", "atomicExpression");
        ParseResult conditional = parseExpression("if (ready) left else right");
        assertNoErrors("conditional expression", conditional);
        assertDirectParent(conditional, "ifExpression", "atomicExpression");

        ParseResult statements = parseFile("fun body() { value = other\n while (ready) value++ }");
        assertNoErrors("statement-only assignment and loop", statements);

        for (String removed : List.of(
                "disjunction", "conjunction", "equalityComparison", "namedInfix", "elvisExpression",
                "infixFunctionCall", "rangeExpression", "additiveExpression", "multiplicativeExpression",
                "typeRHS", "conditionalExpression")) {
            if (ruleIndex(precedence.parser, removed) >= 0) {
                throw new AssertionError("Expression wrapper should be removed: " + removed);
            }
        }
        System.out.println("Expression hierarchy checks passed.");
    }

    private static ParseResult parseExpression(String source) {
        return parse(source, false);
    }

    private static ParseResult parseFile(String source) {
        return parse(source, true);
    }

    private static ParseResult parse(String source, boolean file) {
        List<String> errors = new ArrayList<>();
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        ParserRuleContext tree = file ? parser.kotlinFile() : parser.expression();
        if (parser.getCurrentToken().getType() != Token.EOF) {
            errors.add("Unconsumed input " + parser.getCurrentToken().getText());
        }
        return new ParseResult(parser, tree, errors);
    }

    private static String shape(ParseTree tree, Parser parser) {
        if (!(tree instanceof ParserRuleContext context)) {
            return tree.getText();
        }
        String rule = ruleName(context, parser);
        if (rule.equals("expression")) {
            List<ParserRuleContext> children = directChildren(context, parser, "expression");
            if (children.size() == 2) {
                return binary(operator(context, parser), shape(children.get(0), parser), shape(children.get(1), parser));
            }
            List<ParserRuleContext> comparisons = directChildren(context, parser, "comparison");
            if (!comparisons.isEmpty()) {
                return shape(comparisons.get(0), parser);
            }
            return shape(directChild(context, parser, "infixExpression"), parser);
        }
        if (rule.equals("comparison")) {
            List<ParserRuleContext> children = directChildren(context, parser, "infixExpression");
            if (children.size() == 2) {
                return binary(operator(context, parser), shape(children.get(0), parser), shape(children.get(1), parser));
            }
            throw new AssertionError("Expected comparison operands in " + context.toStringTree(parser));
        }
        if (rule.equals("infixExpression")) {
            List<ParserRuleContext> children = directChildren(context, parser, "infixExpression");
            if (!children.isEmpty()) {
                String operation = operator(context, parser);
                if (operation.equals("is") || operation.equals("!is")) {
                    return binary(operation, shape(children.get(0), parser), directChild(context, parser, "type").getText());
                }
                if (children.size() == 2) {
                    return binary(operation, shape(children.get(0), parser), shape(children.get(1), parser));
                }
            }
            ParserRuleContext prefix = directChild(context, parser, "prefixUnaryExpression");
            String result = prefix.getText();
            List<ParserRuleContext> operations = directChildren(context, parser, "typeOperation");
            List<ParserRuleContext> types = directChildren(context, parser, "type");
            for (int i = 0; i < operations.size(); i++) {
                result = binary(operations.get(i).getText(), result, types.get(i).getText());
            }
            return result;
        }
        if (rule.equals("parenthesizedExpression")) {
            return "(" + shape(directChild(context, parser, "expression"), parser) + ")";
        }
        return context.getText();
    }

    private static String operator(ParserRuleContext context, Parser parser) {
        for (int i = 0; i < context.getChildCount(); i++) {
            ParseTree child = context.getChild(i);
            if (child instanceof TerminalNode terminal) {
                String text = terminal.getText().strip();
                if (List.of("||", "&&", "?:", "..", "<", ">", "<=", ">=", "==", "!=", "===", "!==")
                        .contains(text)) {
                    return text;
                }
            } else if (child instanceof ParserRuleContext rule) {
                String name = ruleName(rule, parser);
                if (List.of("equalityOperation", "comparisonOperator", "inOperator", "isOperator",
                        "additiveOperator", "multiplicativeOperation", "typeOperation", "simpleIdentifier")
                        .contains(name)) {
                    return rule.getText().strip();
                }
            }
        }
        throw new AssertionError("No direct operator in " + context.toStringTree(parser));
    }

    private static String binary(String operator, String left, String right) {
        return "(" + operator + " " + left + " " + right + ")";
    }

    private static void assertShape(ParseResult result, String expected) {
        assertNoErrors("expression shape", result);
        String actual = shape(result.tree, result.parser);
        if (!actual.equals(expected)) {
            throw new AssertionError("Expected expression shape " + expected + " but found " + actual);
        }
    }

    private static void assertDirectParent(ParseResult result, String childRule, String parentRule) {
        for (ParserRuleContext child : findRules(result.tree, result.parser, childRule)) {
            if (child.getParent() instanceof ParserRuleContext parent && ruleName(parent, result.parser).equals(parentRule)) {
                return;
            }
        }
        throw new AssertionError("Expected " + childRule + " to be a direct child of " + parentRule);
    }

    private static void assertDirectChildCount(ParseResult result, String parentRule, String childRule, int expected) {
        int actual = 0;
        for (ParserRuleContext parent : findRules(result.tree, result.parser, parentRule)) {
            actual += directChildren(parent, result.parser, childRule).size();
        }
        if (actual != expected) {
            throw new AssertionError("Expected " + expected + " direct " + childRule + " children of " + parentRule
                    + " but found " + actual);
        }
    }

    private static void assertCount(ParseResult result, String rule, int expected) {
        int actual = findRules(result.tree, result.parser, rule).size();
        if (actual != expected) {
            throw new AssertionError("Expected " + expected + " " + rule + " nodes but found " + actual);
        }
    }

    private static ParserRuleContext directChild(ParserRuleContext context, Parser parser, String rule) {
        List<ParserRuleContext> children = directChildren(context, parser, rule);
        if (children.size() != 1) {
            throw new AssertionError("Expected one direct " + rule + " child in " + context.getText());
        }
        return children.get(0);
    }

    private static List<ParserRuleContext> directChildren(ParserRuleContext context, Parser parser, String rule) {
        List<ParserRuleContext> children = new ArrayList<>();
        for (int i = 0; i < context.getChildCount(); i++) {
            if (context.getChild(i) instanceof ParserRuleContext child && ruleName(child, parser).equals(rule)) {
                children.add(child);
            }
        }
        return children;
    }

    private static List<ParserRuleContext> findRules(ParseTree tree, Parser parser, String rule) {
        List<ParserRuleContext> result = new ArrayList<>();
        if (tree instanceof ParserRuleContext context && ruleName(context, parser).equals(rule)) {
            result.add(context);
        }
        for (int i = 0; i < tree.getChildCount(); i++) {
            result.addAll(findRules(tree.getChild(i), parser, rule));
        }
        return result;
    }

    private static int ruleIndex(Parser parser, String rule) {
        String[] names = parser.getRuleNames();
        for (int i = 0; i < names.length; i++) {
            if (names[i].equals(rule)) {
                return i;
            }
        }
        return -1;
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
}