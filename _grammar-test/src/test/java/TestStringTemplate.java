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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class TestStringTemplate {

    private static final Logger LOGGER = LoggerFactory.getLogger(TestStringTemplate.class);

    private static File[] ok = new File("../stringtemplate/examples")
            .listFiles(pathname -> pathname.isFile());

    private static File[] gfiles = new File[]{
            new File("../stringtemplate/STGLexer.g4"),
            new File("../stringtemplate/STParser.g4"),
            new File("../stringtemplate/LexBasic.g4"),
            new File("../stringtemplate/STGParser.g4"),
            new File("../stringtemplate/STLexer.g4")
    };


    @Test
    public void test() {

        // Exam
        ToolCustomizer tc = t -> t.genPackage = "org.antlr.parser.st4";

        GenericParser gp = null;
        try {
            gp = new GenericParser(tc, gfiles);
        } catch (FileNotFoundException e) {
            assertTrue(false);
        }

        DefaultTreeListener dt = new DefaultTreeListener();

        gp.setListener(dt);

        try {
            File util = new File
                    ("../stringtemplate/src/main/java/org/antlr/parser/st4/LexerAdaptor.java");
            gp.addUtilityJavaFiles(util);
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


        gp.setParserName("org.antlr.parser.st4.STParser");
        gp.setLexerName("org.antlr.parser.st4.STLexer");

        assertTrue(compile);

        for (File f : ok) {
            LOGGER.info("parse {}", f.getAbsoluteFile());
            try {
                gp.parse(f,"template", GenericParser.CaseSensitiveType.NONE);
            } catch (IllegalWorkflowException |
                    FileNotFoundException |
                    ParsingException e) {
                Assert.assertTrue(false);
            }
        }

    }


}
