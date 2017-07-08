import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestCool {

    private static File gfile = new File("../cool/COOL.g4");
    private static File [] ok = new File("../cool/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, gfile));
    }
}
