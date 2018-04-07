import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestBasic {

    private static File gfile = new File("../basic/jvmBasic.g4");
    private static File [] ok = new File("../basic/examples").listFiles((dir, name) -> !dir.isDirectory());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "prog", gfile));
    }
}
