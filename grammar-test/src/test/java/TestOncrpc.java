import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestOncrpc {

    private static File [] ok = new File("../oncrpc/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File [] gfiles =  new File [] {
            new File("../oncrpc/oncrpcv2.g4"),
            new File("../oncrpc/xdr.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfiles));
    }


}
