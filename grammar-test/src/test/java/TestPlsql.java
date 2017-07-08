import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.GenericParser;
import org.snt.inmemantlr.exceptions.CompilationException;
import org.snt.inmemantlr.exceptions.IllegalWorkflowException;
import org.snt.inmemantlr.listener.DefaultTreeListener;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;

import static org.junit.Assert.assertTrue;

public class TestPlsql {

    private static final Logger LOGGER = LoggerFactory.getLogger(TestPlsql.class);

    private static File [] ok1 = new File("../plsql/examples").listFiles(new
                                                                        FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File [] ok2 = new File("../plsql/examples-sql-script")
            .listFiles(new
                                                                        FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File gfile =  new File("../plsql/plsql.g4");

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

        boolean compile;
        try {
            gp.compile();
            compile = true;
        } catch (CompilationException e) {
            compile = false;
        }

        assertTrue(compile);

        for(File f : ok1) {
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

        for(File f : ok2) {
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
