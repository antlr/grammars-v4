import org.junit.Assert;
import org.junit.Test;


import java.io.File;
import java.io.FileFilter;

public class TestScss {

    private static File [] ok = new File("../scss/testsrc").listFiles(new FileFilter
            () {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File [] gfiles = new File [] {
            new File("../scss/ScssLexer.g4"),
            new File("../scss/ScssParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfiles));
    }

}
