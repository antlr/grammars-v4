import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snt.inmemantlr.GenericParser;
import org.snt.inmemantlr.exceptions.CompilationException;
import org.snt.inmemantlr.exceptions.IllegalWorkflowException;
import org.snt.inmemantlr.listener.DefaultTreeListener;
import org.snt.inmemantlr.tree.Ast;

import java.io.File;
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

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
                    LOGGER.error(e.getMessage());
                    return false;
                }
            } catch (IllegalWorkflowException e) {
                LOGGER.error(e.getMessage());
                return false;
            }
        }
        return true;
    }

    public static Map<String, Ast> runAndGetAsts(File[] ok, File... gfile) {
        GenericParser gp = create(gfile);

        Map<String,Ast> ret = new HashMap<String, Ast>();

        for (File f : ok) {
            LOGGER.info("parse {}", f.getAbsoluteFile());
            try {
                try {
                    gp.parse(f);
                } catch (FileNotFoundException e) {
                    LOGGER.error(e.getMessage());
                    return null;
                }
            } catch (IllegalWorkflowException e) {
                LOGGER.error(e.getMessage());
                return null;
            }

            Path p = Paths.get(f.toURI());

            DefaultTreeListener l = (DefaultTreeListener)gp.getListener();

            ret.put(p.getFileName().toString(), l.getAst());
        }

        return ret;
    }

    public static boolean run(File... gfile) {
        GenericParser gp = create(gfile);
        if (gp == null)
            return false;
        return true;
    }


}
