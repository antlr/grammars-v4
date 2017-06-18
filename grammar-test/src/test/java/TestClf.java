import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestClf {

    private static File gfile = new File("../clf/clf.g4");
    private static File [] ok = new File("../clf/examples").listFiles(new FileFilter() {
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
