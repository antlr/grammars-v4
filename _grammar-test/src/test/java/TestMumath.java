import org.junit.Assert;
import org.junit.Test;

import java.io.File;


public class TestMumath {

    private static File [] ok = new File("../mumath/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../mumath/mumath.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "program", gfile));
    }

}
