import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestTnsnames {

    private static File [] ok = new File("../tnsnames/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File [] gfiles =  new File [] {
            new File("../tnsnames/tnsnamesLexer.g4"),
            new File("../tnsnames/tnsnamesParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfiles));
    }


}
