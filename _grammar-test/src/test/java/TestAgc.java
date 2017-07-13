import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestAgc {


    private static File gfile = new File("../agc/agc.g4");
    private static File [] ok = new File("../agc/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "prog", gfile));
    }
}
