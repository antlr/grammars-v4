import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestDot {

    private static File gfile = new File("../dot/DOT.g4");
    private static File [] ok = new File("../dot/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "graph", gfile));
    }
}
