import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestVb6 {

    private static File [] ok = new File("../vb6/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File gfile =  new File("../vb6/VisualBasic6.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }

}
