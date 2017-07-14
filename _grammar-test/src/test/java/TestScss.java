import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestScss {

    private static File [] ok = new File("../scss/testsrc").listFiles(pathname -> pathname.isFile());

    private static File [] gfiles = new File [] {
            new File("../scss/ScssLexer.g4"),
            new File("../scss/ScssParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "stylesheet", gfiles));
    }

}
