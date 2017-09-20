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

import static junit.framework.TestCase.assertTrue;

public class TestR {

    private static final Logger LOGGER = LoggerFactory.getLogger(TestR.class);

    private static File [] ok = new File("../r/examples").listFiles(pathname
            -> pathname.isFile());

    private static File [] gfiles = new File [] {
            new File("../r/RFilter.g4"),
            new File("../r/R.g4")
    };


    @Test
    public void test(){
        GenericParser gp = null;
        try {
            gp = new GenericParser(gfiles);
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

        gp.setParserName("RParser");
        gp.setLexerName("RLexer");

        Assert.assertTrue(compile);

        for (File f : ok) {
            LOGGER.info("parse {}", f.getAbsoluteFile());
            try {
                gp.parse(f, "prog", GenericParser.CaseSensitiveType.NONE);
            } catch (IllegalWorkflowException |
                    FileNotFoundException |
                    ParsingException e) {
                Assert.assertTrue(false);
            }
        }
    }

}
