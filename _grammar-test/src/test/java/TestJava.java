import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.snt.inmemantlr.GenericParser;
import org.snt.inmemantlr.listener.DefaultTreeListener;

import java.io.File;

public class TestJava {

    private static File [] ok = new File("../java/examples").listFiles(File::isFile);

    private static File[] gfiles = new File[]{
            new File("../java/JavaLexer.g4"),
            new File("../java/JavaParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "compilationUnit", gfiles));
    }

    /**
     * around 66 seconds [linux, intel i7, ssd] (measured on branch master at c17c753412d4c2e5e25cf1b433977f0bf7098c4f)
     */
    @Ignore
    @Test
    public void general_performance() throws Exception {
        parseMultipleTimes(
                100,
                new File("../java/examples/AllInOne7.java"),
                new File("../java/examples/AllInOne8.java"),
                new File("../java/examples/ManyStringsConcat.java")
        );
    }

    /**
     * around 40 seconds [linux, intel-i7, ssd] (47 seconds on branch master at c17c753412d4c2e5e25cf1b433977f0bf7098c4f)
     */
    @Ignore
    @Test
    public void performance_of_method_call_parsing() throws Exception {
        parseMultipleTimes(100, new File("../java/examples/ManyMethodCalls.java"));
    }

    private void parseMultipleTimes(final int iterations, File ... files) throws Exception {
        GenericParser gp = new GenericParser(gfiles);
        DefaultTreeListener t = new DefaultTreeListener();
        gp.setListener(t);
        gp.compile();
        DefaultTreeListener dt = new DefaultTreeListener();
        gp.setListener(dt);
        for (File file : files) {
            for (int i = 0; i < iterations; i++) {
                gp.parse(file, "compilationUnit", GenericParser.CaseSensitiveType.NONE);
            }
        }
    }

}
