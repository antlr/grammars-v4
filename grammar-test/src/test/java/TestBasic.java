import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FilenameFilter;

public class TestBasic {

    private static File gfile = new File("../basic/jvmBasic.g4");
    private static File [] ok = new File("../basic/examples").listFiles(new FilenameFilter() {
        @Override
        public boolean accept(File dir, String name) {
            return !dir.isDirectory();
        }
    });

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, gfile));
    }
}
