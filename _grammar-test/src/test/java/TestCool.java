import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestCool {

    private static File gfile = new File("../cool/COOL.g4");
    private static File [] ok = new File("../cool/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "program", gfile));
    }
}
