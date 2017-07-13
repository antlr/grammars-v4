import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestLogo {

    private static File [] ok = new File("../logo/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../logo/logo.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "prog", gfile));
    }

}
