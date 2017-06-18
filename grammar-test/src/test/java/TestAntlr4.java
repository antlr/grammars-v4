import org.antlr.v4.Tool;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.GenericParser;
import org.snt.inmemantlr.exceptions.CompilationException;
import org.snt.inmemantlr.exceptions.IllegalWorkflowException;
import org.snt.inmemantlr.listener.DefaultTreeListener;
import org.snt.inmemantlr.tool.ToolCustomizer;

import java.io.File;
import java.io.FileNotFoundException;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class TestAntlr4 {

    private static final Logger LOGGER = LoggerFactory.getLogger(TestAntlr4.class);

    private static File lexerAdaptor = new File
            ("../antlr4/src/main/java/org/antlr/parser/antlr4/LexerAdaptor.java");

    private static File [] ok = new File("../antlr4/examples").listFiles();

    private static File [] gfile =  new File [] {
            new File("../antlr4/ANTLRv4Lexer.g4"),
            new File("../antlr4/ANTLRv4Parser.g4"),
            new File("../antlr4/LexBasic.g4")
    };

    @Test
    public void test() {

        ToolCustomizer tc = new ToolCustomizer() {
            @Override
            public void customize(Tool t) {
                t.genPackage =  "org.antlr.parser.antlr4";
            }
        };

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

        for(File f : ok) {
            LOGGER.info("parse {}", f.getAbsoluteFile());
            try {
                try {
                    gp.parse(f);
                } catch (FileNotFoundException e) {
                    Assert.assertTrue(false);
                }
            } catch (IllegalWorkflowException e) {
                Assert.assertTrue(false);
            }
        }
    }


}
