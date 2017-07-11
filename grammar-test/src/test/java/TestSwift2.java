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
import java.io.FileFilter;
import java.io.FileNotFoundException;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class TestSwift2 {

    private static final Logger LOGGER = LoggerFactory.getLogger(TestSwift2.class);

    private static File [] ok = new File("../swift2/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File gfile =  new File("../swift2/Swift2.g4");

    @Test
    public void test() {


        GenericParser gp = null;
        try {
            gp = new GenericParser(gfile);
        } catch (FileNotFoundException e) {
            assertTrue(false);
        }

        DefaultTreeListener dt = new DefaultTreeListener();

        gp.setListener(dt);

        try {
            File util = new File
                    ("../swift2/src/main/java/SwiftSupport.java");
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
                gp.parse(f, "top_level", GenericParser.CaseSensitiveType.NONE);
            } catch (IllegalWorkflowException |
                    FileNotFoundException |
                    ParsingException e) {
                Assert.assertTrue(false);
            }
        }
    }


}
