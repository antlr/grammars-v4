import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestVhdl {

    private static File [] ok = new File("../vhdl/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File gfile =  new File("../vhdl/vhdl.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfile));
    }


}
