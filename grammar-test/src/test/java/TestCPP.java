import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestCPP {

    private static File gfile = new File("../cpp/CPP14.g4");
    private static File [] ok = new File("../cpp/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "translationunit", gfile));
    }

}
