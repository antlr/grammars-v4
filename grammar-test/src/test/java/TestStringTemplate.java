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
import java.io.FileFilter;
import java.io.FileNotFoundException;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class TestStringTemplate {

    private static final Logger LOGGER = LoggerFactory.getLogger(TestStringTemplate.class);

    private static File [] ok = new File("../stringtemplate/examples")
            .listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File [] gfiles = new File [] {
            new File("../stringtemplate/STGLexer.g4"),
            new File("../stringtemplate/LexUnicode.g4"),
            new File("../stringtemplate/STParser.g4"),
            new File("../stringtemplate/LexBasic.g4"),
            new File("../stringtemplate/LexBasic.g4"),
            new File("../stringtemplate/STGParser.g4"),
            new File("../stringtemplate/STLexer.g4")
    };


    @Test
    public void test() {

        // Exam
        ToolCustomizer tc = new ToolCustomizer() {
            @Override
            public void customize(Tool t) {
                t.genPackage = "org.antlr.parser.st4";
            }
        };

        GenericParser gp = null;
        try {
            gp = new GenericParser(tc,gfiles);
        } catch (FileNotFoundException e) {
            assertTrue(false);
        }

        DefaultTreeListener dt = new DefaultTreeListener();

        gp.setListener(dt);

        try {
            File util = new File
                    ("../stringtemplate//src/main/java/org/antlr/parser/st4" +
                            "/LexerAdaptor.java");
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
