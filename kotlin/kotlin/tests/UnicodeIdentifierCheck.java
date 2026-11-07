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

final class UnicodeIdentifierCheck {
    private static final String COMPILER_BASELINE = "Kotlin 2.5.0-Beta2";
    private static final String GEORGIAN_MTVRULI = codePoint(0x1C90);
    private static final String CJK_EXTENSION_G = codePoint(0x30000);
    private static final String ADLAM = codePoint(0x1E900);
    private static final String DESERET = codePoint(0x10400);

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
        assertSingleToken("ß", "Identifier");
        assertSingleToken("ʰ", "Identifier");
        assertSingleToken("ǅ", "Identifier");
        assertSingleToken(GEORGIAN_MTVRULI, "Identifier");
        assertSingleToken(ADLAM, "Identifier");
        assertSingleToken(DESERET, "Identifier");
        assertSingleToken("Ⅻ", "Identifier");
        assertSingleToken("name١", "Identifier");
        assertSingleToken("_name", "Identifier");
        assertSingleToken("١", "UNICODE_CLASS_ND");
        assertNotIdentifier("١");
        assertHasErrors("leading Unicode digit in identifier", parse("val ١ = 1"));

        assertIdentifierContext("parentheses", "(" + GEORGIAN_MTVRULI + ")", GEORGIAN_MTVRULI, "Identifier");
        assertIdentifierContext("brackets", "values[" + ADLAM + "]", ADLAM, "Identifier");
        assertIdentifierContext("label reference", "@" + GEORGIAN_MTVRULI, "@" + GEORGIAN_MTVRULI, "LabelReference");
        assertIdentifierContext("label definition", GEORGIAN_MTVRULI + "@", GEORGIAN_MTVRULI + "@", "LabelDefinition");
        assertIdentifierContext("field identifier", "$" + ADLAM, "$" + ADLAM, "FieldIdentifier");
        assertIdentifierContext("short template", "\"$" + GEORGIAN_MTVRULI + "\"", "$" + GEORGIAN_MTVRULI, "LineStrRef");
        assertIdentifierContext("line template interpolation", "\"${" + DESERET + "}\"", DESERET, "Identifier");
        assertIdentifierContext("raw template interpolation", "\"\"\"${" + ADLAM + "}\"\"\"", ADLAM, "Identifier");

        assertRejectedCodePoint(codePoint(0x0301), "combining mark");
        assertNotSingleIdentifier("e" + codePoint(0x0301), "combining mark after a letter");
        assertRejectedCodePoint(codePoint(0x1E4D0), "Unicode 15 Nag Mundari letter beyond " + COMPILER_BASELINE + "'s Unicode 12.1 baseline");
        assertRejectedCodePoint(CJK_EXTENSION_G, "Unicode 13 CJK Extension G letter beyond " + COMPILER_BASELINE + "'s Unicode 12.1 baseline");
        assertNoErrors("Unicode identifier fixture", parse(Files.readString(Path.of("examples/kotlinFile/unicode_identifiers.kt"))));
        assertNoErrors("Nl policy", parse("val Ⅻ = 12"));
        System.out.println("Unicode identifier checks passed (compiler baseline: " + COMPILER_BASELINE + ").");
    }

    private static String codePoint(int value) {
        return new String(Character.toChars(value));
    }

    private static void assertSingleToken(String source, String expectedType) {
        LexResult result = lex(source);
        assertNoErrors("lexing " + printable(source), result.errors);
        if (result.tokens.size() != 1) {
            throw new AssertionError("Expected one " + expectedType + " token for " + printable(source) + " but found " + describe(result));
        }
        assertTokenType(result, result.tokens.get(0), source, expectedType);
    }

    private static void assertNotIdentifier(String source) {
        LexResult result = lex(source);
        if (result.tokens.stream().anyMatch(token -> "Identifier".equals(symbolicName(result, token)))) {
            throw new AssertionError("Expected a non-start digit, not an identifier: " + describe(result));
        }
    }

    private static void assertNotSingleIdentifier(String source, String label) {
        LexResult result = lex(source);
        if (result.tokens.size() == 1 && "Identifier".equals(symbolicName(result, result.tokens.get(0)))
            && source.equals(result.tokens.get(0).getText())) {
            throw new AssertionError(label + " was accepted as an identifier: " + describe(result));
        }
    }

    private static void assertIdentifierContext(String label, String source, String text, String expectedType) {
        LexResult result = lex(source);
        assertNoErrors(label, result.errors);
        for (Token token : result.tokens) {
            if (text.equals(token.getText())) {
                assertTokenType(result, token, text, expectedType);
                return;
            }
        }
        throw new AssertionError(label + ": did not find " + printable(text) + " in " + describe(result));
    }

    private static void assertTokenType(LexResult result, Token token, String text, String expectedType) {
        String actualType = symbolicName(result, token);
        if (!expectedType.equals(actualType) || !text.equals(token.getText())) {
            throw new AssertionError("Expected " + expectedType + " token for " + printable(text) + " but found " + actualType + " "
                + printable(token.getText()));
        }
    }

    private static void assertRejectedCodePoint(String source, String label) {
        LexResult result = lex(source);
        if (result.errors.isEmpty()) {
            throw new AssertionError(label + " should not be part of the frozen identifier categories: " + describe(result));
        }
        if (result.tokens.stream().anyMatch(token -> "Identifier".equals(symbolicName(result, token)))) {
            throw new AssertionError(label + " unexpectedly became an identifier: " + describe(result));
        }
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

    private static String symbolicName(LexResult result, Token token) {
        return result.lexer.getVocabulary().getSymbolicName(token.getType());
    }

    private static String describe(LexResult result) {
        return result.tokens.stream()
            .map(token -> symbolicName(result, token) + " " + printable(token.getText()))
            .toList().toString();
    }

    private static String printable(String value) {
        return value.codePoints()
            .mapToObj(codePoint -> String.format("U+%04X", codePoint))
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
            throw new AssertionError(label + " should be rejected");
        }
    }
}