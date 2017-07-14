import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestCreole {

    private static File gfile = new File("../creole/creole.g4");
    private static File [] ok = new File("../creole/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "document", gfile));
    }
}
