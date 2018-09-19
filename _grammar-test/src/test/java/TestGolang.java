import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestGolang {

    private static File gfile = new File("../golang/Golang.g4");
    private static File [] ok = new File("../golang/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "sourceFile", gfile));
    }
}
