import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestVba {

    private static File [] ok = new File("../vba/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File gfile =  new File("../vba/vba.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }

}
