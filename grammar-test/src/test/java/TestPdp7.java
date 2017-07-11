import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestPdp7 {

    private static File [] ok = new File("../pdp7/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../pdp7/pdp7.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "prog", gfiles));
    }


}
