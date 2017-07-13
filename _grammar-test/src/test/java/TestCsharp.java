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

import static org.junit.Assert.assertTrue;

public class TestCsharp {

    private static final Logger LOGGER = LoggerFactory.getLogger(TestCsharp.class);

    private static File[] ok = new File("../csharp/examples").listFiles();

    private static File[] gfile = new File[]{
            new File("../csharp/CSharpLexer.g4"),
            new File("../csharp/CSharpParser.g4"),
    };

    @Test
    public void test() {

        ToolCustomizer tc = t -> t.genPackage = "org.antlr.parser.antlr4";

        GenericParser gp = null;
        try {
            gp = new GenericParser(tc, gfile);
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

        for (File f : ok) {
            LOGGER.info("parse {}", f.getAbsoluteFile());
            try {
                gp.parse(f, "compilation_unit", GenericParser.CaseSensitiveType.NONE);
            } catch (IllegalWorkflowException |
                    FileNotFoundException |
                    ParsingException e) {
                Assert.assertTrue(false);
            }
        }
    }


}
