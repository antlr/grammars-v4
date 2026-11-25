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

final class NumericLiteralCheck {
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
        for (String literal : List.of("1e3", "1E-3", "1_2e+3_4", ".5", "0.5", "00.5", "001.25", "001e2", "001f",
                 "00.5F", "1.25e-3F", "0e+1")) {
            assertSingleToken(literal, "RealLiteral");
        }
        for (String literal : List.of("0", "1", "12", "1_2", "1_2_3")) {
            assertSingleToken(literal, "IntegerLiteral");
        }
        for (String literal : List.of("0L", "1L", "1_2L", "0x1L", "0xCA_FEL", "0b1L", "0b10_01L")) {
            assertSingleToken(literal, "LongLiteral");
        }
        for (String literal : List.of("0x1", "0XCA_FE")) {
            assertSingleToken(literal, "HexLiteral");
        }
        for (String literal : List.of("0b0", "0B10_01")) {
            assertSingleToken(literal, "BinLiteral");
        }
        for (String literal : List.of("0u", "1u", "42U", "1_000u", "0xFFu", "0XCA_FEU", "0b10u", "0B10_01U",
                 "0UL", "42uL", "1_000UL", "0xFFUL", "0XCA_FEuL", "0b10uL", "0B10_01UL")) {
            assertSingleToken(literal, "UnsignedLiteral");
        }

        String unicodeDigit = new String(Character.toChars(0x0661));
        for (String literal : List.of("0x1_", "0b1_", "12_", "01", "01L", "01u", "1e3L", "1.0u", "1e3U",
                 "1ul", "1Ul", "1Lu", "1uu", "0x1_u", "0b1_U", unicodeDigit, "1" + unicodeDigit)) {
            assertNotCompleteNumericLiteral(literal);
            assertHasErrors("invalid numeric initializer " + literal, parse("val numeric = " + literal));
        }

        assertSingleToken("name" + unicodeDigit, "Identifier");
        assertContextToken("default mode", "val value = 1e3", "1e3", "RealLiteral");
        assertContextToken("parentheses", "val value = (1e3)", "1e3", "RealLiteral");
        assertContextToken("brackets", "val value = [1e3]", "1e3", "RealLiteral");
        assertContextToken("line template", "val value = \"${1e3}\"", "1e3", "RealLiteral");
        assertContextToken("multiline template", "val value = \"\"\"${1e3}\"\"\"", "1e3", "RealLiteral");
        assertContextToken("default unsigned literal", "val value = 1uL", "1uL", "UnsignedLiteral");
        assertContextToken("parenthesized unsigned literal", "val value = (1uL)", "1uL", "UnsignedLiteral");
        assertContextToken("bracketed unsigned literal", "val value = [1uL]", "1uL", "UnsignedLiteral");
        assertContextToken("line-template unsigned literal", "val value = \"${1uL}\"", "1uL", "UnsignedLiteral");
        assertContextToken("multiline-template unsigned literal", "val value = \"\"\"${1uL}\"\"\"", "1uL", "UnsignedLiteral");
        for (String source : List.of("val value = 1uL", "val value = (1uL)", "val value = [1uL]",
                 "val value = \"${1uL}\"", "val value = \"\"\"${1uL}\"\"\"")) {
            assertNoErrors("unsigned literal parse context " + source, parse(source));
        }
        assertContextToken("default open-ended range", "val value = 1..<2", "..<", "RANGE_UNTIL");
        assertContextToken("parenthesized open-ended range", "val value = (1..<2)", "..<", "RANGE_UNTIL");
        assertContextToken("bracketed open-ended range", "val value = [1..<2]", "..<", "RANGE_UNTIL");
        assertContextToken("line-template open-ended range", "val value = \"${1..<2}\"", "..<", "RANGE_UNTIL");
        assertContextToken("multiline-template open-ended range", "val value = \"\"\"${1..<2}\"\"\"", "..<", "RANGE_UNTIL");

        assertTokenSequence("1..2", List.of("IntegerLiteral", "RANGE", "IntegerLiteral"));
        assertTokenSequence("1..<2", List.of("IntegerLiteral", "RANGE_UNTIL", "IntegerLiteral"));
        assertTokenSequence("1.. <2", List.of("IntegerLiteral", "RANGE", "LANGLE", "IntegerLiteral"));
        assertNoErrors("numeric literal fixture", parse(Files.readString(Path.of("examples/kotlinFile/numeric_literals.kt"))));
        System.out.println("Numeric literal checks passed.");
    }

    private static LexResult lex(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        return new LexResult(lexer, lexer.getAllTokens(), errors);
    }

    private static List<String> parse(String source) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(source));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        parser.kotlinFile();
        return errors;
    }

    private static void assertSingleToken(String source, String expectedType) {
        LexResult result = lex(source);
        assertNoErrors("lexing " + source, result.errors);
        if (result.tokens.size() != 1) {
            throw new AssertionError("Expected one " + expectedType + " token for " + source + " but found " + describe(result));
        }
        Token token = result.tokens.get(0);
        String actualType = result.lexer.getVocabulary().getSymbolicName(token.getType());
        if (!expectedType.equals(actualType) || !source.equals(token.getText())) {
            throw new AssertionError("Expected " + expectedType + " token for " + source + " but found " + actualType + " " + token.getText());
        }
    }

    private static void assertNotCompleteNumericLiteral(String source) {
        LexResult result = lex(source);
        if (result.errors.isEmpty() && result.tokens.size() == 1 && source.equals(result.tokens.get(0).getText())) {
            String type = result.lexer.getVocabulary().getSymbolicName(result.tokens.get(0).getType());
            if (List.of("IntegerLiteral", "LongLiteral", "UnsignedLiteral", "RealLiteral", "HexLiteral", "BinLiteral").contains(type)) {
                throw new AssertionError("Invalid numeral was recognized as a complete " + type + ": " + source);
            }
        }
    }

    private static void assertContextToken(String label, String source, String literal, String expectedType) {
        LexResult result = lex(source);
        assertNoErrors(label, result.errors);
        for (Token token : result.tokens) {
            if (literal.equals(token.getText())) {
                String actualType = result.lexer.getVocabulary().getSymbolicName(token.getType());
                if (!expectedType.equals(actualType)) {
                    throw new AssertionError(label + ": expected " + expectedType + " but found " + actualType);
                }
                return;
            }
        }
        throw new AssertionError(label + ": did not find literal token " + literal + " in " + describe(result));
    }

    private static void assertTokenSequence(String source, List<String> expectedTypes) {
        LexResult result = lex(source);
        assertNoErrors("lexing " + source, result.errors);
        List<String> actualTypes = result.tokens.stream()
            .map(token -> result.lexer.getVocabulary().getSymbolicName(token.getType()))
            .toList();
        if (!expectedTypes.equals(actualTypes)) {
            throw new AssertionError("Expected tokens " + expectedTypes + " for " + source + " but found " + actualTypes);
        }
    }

    private static String describe(LexResult result) {
        return result.tokens.stream()
            .map(token -> result.lexer.getVocabulary().getSymbolicName(token.getType()) + " " + token.getText())
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

    private static void assertHasErrors(String label, List<String> errors) {
        if (errors.isEmpty()) {
            throw new AssertionError(label + " should produce a lexing or parsing error");
        }
    }
}
