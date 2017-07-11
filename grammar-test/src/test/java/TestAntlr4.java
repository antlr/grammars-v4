import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.GenericParser;
import org.snt.inmemantlr.exceptions.CompilationException;
import org.snt.inmemantlr.exceptions.IllegalWorkflowException;
import org.snt.inmemantlr.exceptions.ParsingException;
import org.snt.inmemantlr.listener.DefaultTreeListener;
import org.snt.inmemantlr.tool.ToolCustomizer;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class TestAntlr4 {

    private static final Logger LOGGER = LoggerFactory.getLogger(TestAntlr4.class);

    private static File lexerAdaptor = new File
            ("../antlr4/src/main/java/org/antlr/parser/antlr4/LexerAdaptor.java");

    private static File[] ok = Arrays.stream(new File("../antlr4/examples")
            .listFiles()).filter(f -> !f.getName().endsWith("errors"))
            .toArray(size -> new File[size]);

    private static File[] gfile = new File[]{
            new File("../antlr4/ANTLRv4Lexer.g4"),
            new File("../antlr4/ANTLRv4Parser.g4"),
            new File("../antlr4/LexBasic.g4")
    };

    @Test
    public void test() {

        ToolCustomizer tc = t -> t.genPackage = "org.antlr.parser.antlr4";

        GenericParser gp = null;
        try {
            gp = new GenericParser(tc, gfile);
        } catch (FileNotFoundException e) {
            assertTrue(false);
        }

        DefaultTreeListener dt = new DefaultTreeListener();

        gp.setListener(dt);

        try {
            gp.addUtilityJavaFiles(lexerAdaptor);
        } catch (FileNotFoundException e) {
            assertFalse(true);
        }

        boolean compile;
        try {
            gp.compile();
            compile = true;
        } catch (CompilationException e) {
            compile = false;
        }

        assertTrue(compile);

        boolean thrown = false;
        boolean erroneous = false;

        for (File f : ok) {

            thrown = false;

            LOGGER.info("parse {}", f.getAbsoluteFile());

            erroneous = f.getName().contains("three.g4");

            try {
                gp.parse(f, "grammarSpec", GenericParser.CaseSensitiveType
                        .NONE);
            } catch (IllegalWorkflowException |
                    FileNotFoundException |
                    ParsingException e) {
                thrown = true;
            }

            Assert.assertEquals(erroneous, thrown);
        }


    }

}
