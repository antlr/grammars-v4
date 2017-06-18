import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestRuby {

    private static File [] ok = new File("../ruby/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File gfile =  new File("../ruby/Corundum.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, gfile));
    }


}
