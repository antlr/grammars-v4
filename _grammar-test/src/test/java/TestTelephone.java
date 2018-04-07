import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestTelephone {

    private static File [] ok = new File("../telephone/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../telephone/telephone.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "number", gfiles));
    }

}
