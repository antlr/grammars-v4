import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestAntlr3 {


    private static File gfile = new File("../antlr3/ANTLRv3.g4");
    private static File [] ok = new File("../antlr3/examples").listFiles(new FileFilter() {
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
