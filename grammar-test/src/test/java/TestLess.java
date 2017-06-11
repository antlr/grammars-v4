import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestLess {

    private static File [] ok = new File("../less/testsrc").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });


    private static File [] gfiles = new File [] {
            new File("../less/LessParser.g4"),
            new File("../less/LessLexer.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfiles));
    }


}
