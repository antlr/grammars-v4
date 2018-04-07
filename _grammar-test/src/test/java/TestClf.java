import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestClf {

    private static File gfile = new File("../clf/clf.g4");
    private static File [] ok = new File("../clf/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "log", gfile));
    }
}
