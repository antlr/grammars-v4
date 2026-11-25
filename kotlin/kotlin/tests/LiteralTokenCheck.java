package org.antlr.grammars;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;

final class LiteralTokenCheck {
    private static final String BACKSLASH = "\\";

    private static final class LexResult {
        private final List<Token> tokens;
        private final List<String> errors;

        private LexResult(List<Token> tokens, List<String> errors) {
            this.tokens = tokens;
            this.errors = errors;
        }
    }

    public static void main(String[] args) {
        String ordinary = "\"a" + BACKSLASH + "n" + BACKSLASH + "t" + BACKSLASH + "u0041" + BACKSLASH + "$"
            + BACKSLASH + "\"" + BACKSLASH + BACKSLASH + "b\"";
        String raw = "\"\"\"" + "before " + BACKSLASH + "$name " + BACKSLASH + "${x} " + BACKSLASH + "\nline "
            + BACKSLASH + "\"\"\"";

        checkValidOrdinaryString(ordinary);
        checkInvalidOrdinaryEscapes();
        checkOrdinaryStringLineBoundaries();
        checkRawStringBackslashes(raw);
        checkCharacterLiterals();
        checkBacktickIdentifiers();
        checkNestedModes(ordinary);
        checkValidSource(ordinary, raw);
        System.out.println("Literal token checks passed.");
    }

    private static void checkValidOrdinaryString(String input) {
        LexResult result = lex(input);
        assertNoErrors("valid ordinary string escapes", result.errors);
        assertHasSequence("valid ordinary string", names(result.tokens), "QUOTE_OPEN", "LineStrText", "LineStrEscapedChar",
            "LineStrEscapedChar", "LineStrEscapedChar", "LineStrEscapedChar", "LineStrEscapedChar", "LineStrEscapedChar",
            "LineStrText", "QUOTE_CLOSE");
        assertCount("ordinary string escapes", names(result.tokens), "LineStrEscapedChar", 6);
    }

    private static void checkInvalidOrdinaryEscapes() {
        for (String escape : List.of("q", "u12G4", "x")) {
            String input = "\"text" + BACKSLASH + escape + " end\"";
            LexResult result = lex(input);
            assertHasErrors("invalid ordinary escape " + escape, result.errors);
            assertNotContainsTokenText("invalid ordinary escape " + escape, result.tokens, "LineStrEscapedChar", BACKSLASH + escape);
        }
    }

    private static void checkOrdinaryStringLineBoundaries() {
        for (String lineBreak : List.of("\n", "\r", "\r\n")) {
            for (String input : List.of("\"before" + lineBreak + "after\"", "\"before" + BACKSLASH + lineBreak + "after\"")) {
                LexResult result = lex(input);
                assertHasErrors("ordinary string line break", result.errors);
            }
        }
    }

    private static void checkRawStringBackslashes(String input) {
        LexResult result = lex(input + " val");
        assertNoErrors("raw string backslashes", result.errors);
        List<String> tokenNames = names(result.tokens);
        assertHasSequence("raw interpolation and closing delimiter", tokenNames, "TRIPLE_QUOTE_OPEN", "MultiLineStrText",
            "MultiLineStrRef", "MultiLineStrText", "MultiLineStrExprStart", "Identifier", "RCURL",
            "MultiLineStrText", "TRIPLE_QUOTE_CLOSE", "VAL");
        Token close = tokenOfType(result.tokens, "TRIPLE_QUOTE_CLOSE");
        assertEquals("raw closing delimiter", "\"\"\"", close.getText());
        Token beforeClose = result.tokens.get(result.tokens.indexOf(close) - 1);
        assertTrue("backslash immediately before raw closing delimiter remains text", beforeClose.getText().endsWith(BACKSLASH));
        assertTrue("backslash before raw newline remains literal text", beforeClose.getText().contains(BACKSLASH + "\n"));
    }

    private static void checkCharacterLiterals() {
        String valid = "'a' '" + BACKSLASH + "n' '" + BACKSLASH + "u0041' '" + BACKSLASH + BACKSLASH + "' '"
            + BACKSLASH + "''";
        LexResult validResult = lex(valid);
        assertNoErrors("valid character escapes", validResult.errors);
        assertCount("valid character literals", names(validResult.tokens), "CharacterLiteral", 5);

        for (String invalid : List.of("'ab'", "'" + BACKSLASH + "'", "'" + BACKSLASH + "q'", "'\n'", "'\r'")) {
            List<String> inputs = List.of(invalid, "(" + invalid + ")", "\"${" + invalid + "}\"");
            for (int i = 0; i < inputs.size(); i++) {
                LexResult result = lex(inputs.get(i));
                assertNotContains("invalid character literal " + printable(invalid) + " in context " + i,
                    names(result.tokens), "CharacterLiteral");
            }
        }
    }

    private static void checkBacktickIdentifiers() {
        LexResult valid = lex("`one line`");
        assertNoErrors("single-line backtick identifier", valid.errors);
        assertCount("single-line backtick identifier", names(valid.tokens), "Identifier", 1);

        for (String lineBreak : List.of("\n", "\r", "\r\n")) {
            String input = "`first" + lineBreak + "second`";
            LexResult result = lex(input);
            assertHasErrors("multiline backtick identifier", result.errors);
            for (Token token : result.tokens) {
                assertTrue("backtick identifier cannot contain a line break", !"Identifier".equals(name(token))
                    || !(token.getText().contains("\n") || token.getText().contains("\r")));
            }
        }
    }

    private static void checkNestedModes(String ordinary) {
        LexResult inside = lex("(" + ordinary + ")");
        assertNoErrors("ordinary string in Inside mode", inside.errors);
        assertHasSequence("ordinary string in Inside mode", names(inside.tokens), "LPAREN", "QUOTE_OPEN", "LineStrText");
        assertHasSequence("ordinary string restores Inside mode", names(inside.tokens), "QUOTE_CLOSE", "RPAREN");

        LexResult insideCharacter = lex("('x')");
        assertNoErrors("character literal in Inside mode", insideCharacter.errors);
        assertHasSequence("character literal in Inside mode", names(insideCharacter.tokens), "LPAREN", "CharacterLiteral", "RPAREN");

        LexResult templateCharacter = lex("\"${'x'}\"");
        assertNoErrors("character literal in StringExpression mode", templateCharacter.errors);
        assertHasSequence("character literal in StringExpression mode", names(templateCharacter.tokens), "LineStrExprStart",
            "CharacterLiteral", "RCURL", "QUOTE_CLOSE");

    }

    private static void checkValidSource(String ordinary, String raw) {
        String source = "val ordinary = " + ordinary + "\n"
            + "val raw = " + raw + "\n"
            + "val chars = listOf('a', '" + BACKSLASH + "n', '" + BACKSLASH + "u0041', '" + BACKSLASH + BACKSLASH + "')\n"
            + "val escaped = `lineName`\n"
            + "val interpolated = \"${'x'}\"\n";
        List<String> errors = parseErrors(source);
        assertNoErrors("valid Kotlin literal source", errors);
    }

    private static LexResult lex(String input) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(input));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        List<Token> tokens = new ArrayList<>(lexer.getAllTokens());
        return new LexResult(tokens, errors);
    }

    private static List<String> parseErrors(String input) {
        KotlinLexer lexer = new KotlinLexer(CharStreams.fromString(input));
        List<String> errors = new ArrayList<>();
        lexer.removeErrorListeners();
        lexer.addErrorListener(errorCollector(errors));
        KotlinParser parser = new KotlinParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(errorCollector(errors));
        parser.kotlinFile();
        return errors;
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

    private static List<String> names(List<Token> tokens) {
        List<String> result = new ArrayList<>();
        for (Token token : tokens) {
            result.add(name(token));
        }
        return result;
    }

    private static String name(Token token) {
        return KotlinLexer.VOCABULARY.getSymbolicName(token.getType());
    }

    private static Token tokenOfType(List<Token> tokens, String type) {
        for (Token token : tokens) {
            if (type.equals(name(token))) {
                return token;
            }
        }
        throw new AssertionError("Missing token " + type + " in " + names(tokens));
    }

    private static void assertNoErrors(String label, List<String> errors) {
        if (!errors.isEmpty()) {
            throw new AssertionError(label + " produced errors: " + errors);
        }
    }

    private static void assertHasErrors(String label, List<String> errors) {
        if (errors.isEmpty()) {
            throw new AssertionError(label + " should produce a lexer error");
        }
    }

    private static void assertHasSequence(String label, List<String> actual, String... expected) {
        for (int start = 0; start <= actual.size() - expected.length; start++) {
            if (actual.subList(start, start + expected.length).equals(Arrays.asList(expected))) {
                return;
            }
        }
        throw new AssertionError(label + " expected sequence " + Arrays.asList(expected) + " in " + actual);
    }

    private static void assertNotContains(String label, List<String> actual, String unexpected) {
        if (actual.contains(unexpected)) {
            throw new AssertionError(label + " unexpectedly contained " + unexpected + ": " + actual);
        }
    }

    private static void assertCount(String label, List<String> actual, String type, int expected) {
        int count = 0;
        for (String item : actual) {
            if (type.equals(item)) {
                count++;
            }
        }
        if (count != expected) {
            throw new AssertionError(label + " expected " + expected + " " + type + " tokens but got " + count + " in " + actual);
        }
    }

    private static void assertNotContainsTokenText(String label, List<Token> tokens, String type, String text) {
        for (Token token : tokens) {
            if (type.equals(name(token)) && text.equals(token.getText())) {
                throw new AssertionError(label + " unexpectedly accepted escape token " + token);
            }
        }
    }

    private static void assertEquals(String label, String expected, String actual) {
        if (!expected.equals(actual)) {
            throw new AssertionError(label + " expected " + expected + " but got " + actual);
        }
    }

    private static void assertTrue(String label, boolean condition) {
        if (!condition) {
            throw new AssertionError(label);
        }
    }

    private static String printable(String input) {
        return input.replace("\r", "\\r").replace("\n", "\\n");
    }
}
