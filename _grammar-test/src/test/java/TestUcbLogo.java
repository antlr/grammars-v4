import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestUcbLogo {

    private static File [] ok = new File("../ucb-logo/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../ucb-logo/UCBLogo.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "parse", gfiles));
    }
}
