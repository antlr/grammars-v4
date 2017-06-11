import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.GenericParser;
import org.snt.inmemantlr.exceptions.CompilationException;
import org.snt.inmemantlr.exceptions.IllegalWorkflowException;
import org.snt.inmemantlr.listener.DefaultTreeListener;

import java.io.File;
import java.io.FileNotFoundException;

public class GrammarTester {

    private static final Logger LOGGER = LoggerFactory.getLogger(GrammarTester.class);

    private static GenericParser create(File... gfile) {
        GenericParser gp = null;
        try {
            gp = new GenericParser(gfile);
        } catch (FileNotFoundException e) {
            LOGGER.error(e.getMessage());
            return null;
        }
        DefaultTreeListener t = new DefaultTreeListener();

        gp.setListener(t);

        try {
            gp.compile();
        } catch (CompilationException e) {
            LOGGER.error(e.getMessage());
            return null;
        }

        return gp;
    }

    public static boolean run(File[] ok, File... gfile) {
        GenericParser gp = create(gfile);

        if (gp == null)
            return false;

        for (File f : ok) {
            LOGGER.info("parse {}", f.getAbsoluteFile());
            try {
                try {
                    gp.parse(f);
                } catch (FileNotFoundException e) {

                }
            } catch (IllegalWorkflowException e) {
                LOGGER.error(e.getMessage());
                return false;
            }
        }

        return true;
    }

    public static boolean run(File... gfile) {
        GenericParser gp = create(gfile);
        if (gp == null)
            return false;
        return true;
    }


}
