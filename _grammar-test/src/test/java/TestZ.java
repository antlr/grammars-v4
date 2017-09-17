import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.GenericParser;
import org.snt.inmemantlr.exceptions.CompilationException;
import org.snt.inmemantlr.exceptions.IllegalWorkflowException;
import org.snt.inmemantlr.exceptions.ParsingException;
import org.snt.inmemantlr.listener.DefaultTreeListener;

import java.io.File;
import java.io.FileNotFoundException;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class TestZ {

    private static final Logger LOGGER = LoggerFactory.getLogger
            (TestZ.class);

    private static File [] ok = new File("../z/examples").listFiles(pathname -> pathname.isFile());


    private static File [] gfiles = new File [] {
            new File("../z/ZLexer.g4"),
            new File("../z/ZParser.g4"),
            new File("../z/ZOperatorParser.g4")
    };

    @Test
    public void test() {

        GenericParser gp = null;
        try {
            gp = new GenericParser(gfiles);
        } catch (FileNotFoundException e) {
            assertTrue(false);
        }

        DefaultTreeListener dt = new DefaultTreeListener();

        gp.setListener(dt);

        try {
            File util1 = new File
                    ("../z/src/main/java/ZOperatorListener.java");
            File util2 = new File
                    ("../z/src/main/java/ZSupport.java");

            gp.addUtilityJavaFiles(util1, util2);

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
                gp.parse(f, "specification", GenericParser.CaseSensitiveType.NONE);
            } catch (IllegalWorkflowException |
                    FileNotFoundException |
                    ParsingException e) {
                Assert.assertTrue(false);
            }
        }
    }


}
