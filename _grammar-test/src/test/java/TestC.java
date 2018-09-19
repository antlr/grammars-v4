import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestC {

    private static File gfile = new File("../c/C.g4");
    private static File [] ok = new File("../c/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "compilationUnit", gfile));
    }
}
