import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestPhp {

    private static File [] ok = new File("../php/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });


    private static File [] gfiles = new File [] {
            new File("../php/PHPLexer.g4"),
            new File("../php/PHPParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfiles));
    }


}
