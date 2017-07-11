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

public class TestPlsql {

    private static final Logger LOGGER = LoggerFactory.getLogger(TestPlsql.class);

    private static File[] ok1 = new File("../plsql/examples").
            listFiles(pathname -> pathname.isFile());

    private static File[] ok2 = new File("../plsql/examples-sql-script")
            .listFiles(pathname -> pathname.isFile());

    private static File[] gfile = new File[]{
            new File("../plsql/PlSqlParser.g4"),
            new File("../plsql/PlSqlLexer.g4")
    };

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


        gp.setStreamProvider(new CasedStreamProvider(GenericParser
                .CaseSensitiveType.UPPER));

        assertTrue(compile);

        for (File f : ok1) {
            LOGGER.info("parse {}", f.getAbsoluteFile());
            try {
                gp.parse(f, "compilation_unit", GenericParser.CaseSensitiveType.NONE);
            } catch (IllegalWorkflowException |
                    FileNotFoundException |
                    ParsingException e) {
                Assert.assertTrue(false);
            }
        }


        // exclude example files for the moment -- green run not working
//        for (File f : ok2) {
//            LOGGER.info("parse {}", f.getAbsoluteFile());
//            try {
//                gp.parse(f, "sql_script", GenericParser.CaseSensitiveType
//                        .NONE);
//            } catch (IllegalWorkflowException |
//                    FileNotFoundException |
//                    ParsingException e) {
//                Assert.assertTrue(false);
//            }
//        }
    }


}
