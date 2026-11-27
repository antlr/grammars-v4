import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.Trees;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

/**
 * Double-quoted text in an expression is a string without ANSI_QUOTES and an identifier with it.
 * The example files run only with the support classes' default, ANSI_QUOTES; this test checks both modes.
 */
@RunWith(Parameterized.class)
public class DoubleQuotedTextTest {

    @Parameterized.Parameters(name = "{0}")
    public static Collection<Object[]> statements() {
        return Arrays.asList(new Object[][] {
            { "SELECT \"a\";" },
            { "SELECT CONCAT(\"a\", b) FROM t;" },
            { "SELECT 1 FROM t WHERE y = \"a\";" },
            { "SELECT JSON_OBJECT(\"k\", v) FROM t;" },
            { "INSERT INTO t VALUES (\"a\", 1);" },
        });
    }

    private final String statement;

    public DoubleQuotedTextTest(String statement) {
        this.statement = statement;
    }

    @Test
    public void stringLiteralWithoutAnsiQuotes() {
        assertDoubleQuotedTextIn(new HashSet<SqlMode>(), MySQLParser.SimpleExprLiteralContext.class);
    }

    @Test
    public void columnRefWithAnsiQuotes() {
        assertDoubleQuotedTextIn(SqlModes.sqlModeFromString("ANSI_QUOTES"), MySQLParser.SimpleExprColumnRefContext.class);
    }

    private void assertDoubleQuotedTextIn(Set<SqlMode> sqlModes, Class<?> expected) {
        final List<String> errors = new ArrayList<String>();
        BaseErrorListener collector = new BaseErrorListener() {
            @Override
            public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line,
                    int charPositionInLine, String msg, RecognitionException e) {
                errors.add(line + ":" + charPositionInLine + " " + msg);
            }
        };

        MySQLLexer lexer = new MySQLLexer(CharStreams.fromString(statement));
        lexer.sqlModes = new HashSet<SqlMode>(sqlModes);
        lexer.removeErrorListeners();
        lexer.addErrorListener(collector);
        MySQLParser parser = new MySQLParser(new CommonTokenStream(lexer));
        parser.sqlModes = new HashSet<SqlMode>(sqlModes);
        parser.removeErrorListeners();
        parser.addErrorListener(collector);
        ParseTree tree = parser.queries();

        assertEquals("syntax errors", new ArrayList<String>(), errors);
        Collection<ParseTree> texts = Trees.findAllTokenNodes(tree, MySQLLexer.DOUBLE_QUOTED_TEXT);
        assertFalse("no double-quoted text in the tree", texts.isEmpty());
        for (ParseTree text : texts) {
            ParseTree expr = text.getParent();
            while (expr != null && !(expr instanceof MySQLParser.SimpleExprContext)) {
                expr = expr.getParent();
            }
            assertEquals(text.getText(), expected, expr == null ? null : expr.getClass());
        }
    }
}
