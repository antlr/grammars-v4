import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestFol {

    private static File gfile = new File("../fol/fol.g4");
    private static File [] ok = new File("../fol/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "condition", gfile));
    }
}
