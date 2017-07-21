import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestKuka {

    private static File [] ok = new File("../kuka/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../kuka/krl.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "module", gfile));
    }

}
