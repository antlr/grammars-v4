package org.antlr.grammars;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;

final class NegatedOperatorCheck {
    private static final class ParseResult {
        private final KotlinParser parser;
        private final List<String> errors;

        private ParseResult(KotlinParser parser, List<String> errors) {
            this.parser = parser;
            this.errors = errors;
        }
    }

    private static final class LexResult {
        private final KotlinLexer lexer;
        private final List<? extends Token> tokens;
        private final List<String> errors;

        private LexResult(KotlinLexer lexer, List<? extends Token> tokens, List<String> errors) {
            this.lexer = lexer;
            this.tokens = tokens;
            this.errors = errors;
        }
    }

    public static void main(String[] args) throws IOException {
        assertOperator("value !is/* block */String", "NOT_IS", "Identifier", "/* block */");
        assertOperator("value !in // line comment\n values", "NOT_IN", "Identifier", "// line comment\n");
        assertOperator("(value !is /* parenthesized */ String)", "NOT_IS", "Identifier", "/* parenthesized */");
        assertOperator("array[value !in /* bracket */ values]", "NOT_IN", "Identifier", "/* bracket */");
        assertOperator("\"${value !is /* line template */ String}\"", "NOT_IS", "Identifier", "/* line template */");
        assertOperator("\"\"\"${1 !in /* raw template */ values}\"\"\"", "NOT_IN", "Identifier", "/* raw template */");
        assertOperator("value !is /* first */\r\n /* second */ String", "NOT_IS", "Identifier", "/* first */");
        assertOperator("1 !in /* first /* nested */ still comment */ values", "NOT_IN", "Identifier", "/* first");

        assertNoCombinedOperator("value !island !inside !inline", "NOT_IS", "NOT_IN");
        assertNoCombinedOperator("value ! /* gap */ is String; value ! /* gap */ in values", "NOT_IS", "NOT_IN");

        assertOperatorsAfterTemplate("\"${value !is /* interpolation */ String}\" + value !in /* restored */ values");

        String fixture = Files.readString(Path.of("examples/kotlinFile/negated_operators.kt"));
        assertNoErrors("negated operators fixture", parse(fixture).errors);
        assertNoErrors("CRLF negated operators fixture", parse(fixture.replace("\n", "\r\n")).errors);
        System.out.println("Negated operator checks passed.");
    }

    private static void assertOperator(String source, String expectedOperator, String expectedOperand, String expectedTrivia) {
        LexResult result = lex(source);
        assertNoErrors("lexing " + source, result.errors);
        for (int i = 0; i < result.tokens.size(); i++) {
            Token token = result.tokens.get(i);
            if (expectedOperator.equals(symbolicName(result, token))) {
                if (i + 1 >= result.tokens.size() || !expectedOperand.equals(symbolicName(result, result.tokens.get(i + 1)))) {
                    throw new AssertionError(expectedOperator + " was not followed by " + expectedOperand + " in " + describe(result));
                }
                if (!token.getText().contains(expectedTrivia)) {
                    throw new AssertionError(expectedOperator + " did not consume " + expectedTrivia + " in " + describe(result));
                }
                return;
            }
        }
        throw new AssertionError("Expected " + expectedOperator + " in " + source + " but found " + describe(result));
    }

    private static void assertNoCombinedOperator(String source, String... operatorTypes) {
        LexResult result = lex(source);
        assertNoErrors("lexing " + source, result.errors);
        for (Token token : result.tokens) {
            for (String operatorType : operatorTypes) {
                if (operatorType.equals(symbolicName(result, token))) {
                    throw new AssertionError("Did not expect " + operatorType + " in " + source + " but found " + describe(result));
                }
            }
        }
    }

    private static void assertOperatorsAfterTemplate(String source) {
        LexResult result = lex(source);
        assertNoErrors("lexing template mode restoration", result.errors);
        int templateOperator = -1;
        int restoredOperator = -1;
        for (int i = 0; i < result.tokens.size(); i++) {
            Token token = result.tokens.get(i);
            if ("NOT_IS".equals(symbolicName(result, token))) {
                templateOperator = i;
            } else if ("NOT_IN".equals(symbolicName(result, token))) {
                restoredOperator = i;
            }
        }
        if (templateOperator < 0 || restoredOperator <= templateOperator) {
            throw new AssertionError("Expected operators inside and after interpolation in " + describe(result));
        }
        if (!"Identifier".equals(symbolicName(result, result.tokens.get(restoredOperator + 1)))) {
            throw new AssertionError("Expected the post-template membership operand after NOT_IN in " + describe(result));
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
        parser.kotlinFile();
        return new ParseResult(parser, errors);
    }

    private static LexResult lex(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        return new LexResult(lexer, lexer.getAllTokens(), errors);
    }

    private static String symbolicName(LexResult result, Token token) {
        return result.lexer.getVocabulary().getSymbolicName(token.getType());
    }

    private static String describe(LexResult result) {
        return result.tokens.stream()
            .map(token -> symbolicName(result, token) + " " + token.getText())
            .toList().toString();
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
}
