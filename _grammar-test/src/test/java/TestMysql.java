import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.GenericParser;
import org.snt.inmemantlr.exceptions.CompilationException;
import org.snt.inmemantlr.exceptions.IllegalWorkflowException;
import org.snt.inmemantlr.exceptions.ParsingException;
import org.snt.inmemantlr.listener.DefaultTreeListener;
import org.snt.inmemantlr.stream.CasedStreamProvider;

import java.io.File;
import java.io.FileNotFoundException;

import static org.junit.Assert.assertTrue;

public class TestMysql {

    private static File [] ok = new File("../mysql/examples").listFiles(pathname -> pathname.isFile());

    private static final Logger LOGGER = LoggerFactory.getLogger(TestMysql.class);

    private static File [] gfiles = new File [] {
            new File("../mysql/MySQLLexer.g4"),
            new File("../mysql/MySQLParser.g4")
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


        gp.setStreamProvider(new CasedStreamProvider(GenericParser
                .CaseSensitiveType.UPPER));

        assertTrue(compile);

        for (File f : ok) {
            LOGGER.info("parse {}", f.getAbsoluteFile());
            try {
                gp.parse(f, "root", GenericParser
                        .CaseSensitiveType.UPPER);
            } catch (IllegalWorkflowException |
                    FileNotFoundException |
                    ParsingException e) {
                Assert.assertTrue(false);
            }
        }
    }


}
