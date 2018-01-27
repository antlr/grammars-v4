import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestPostalcode {

    private static File [] ok = new File("../postalcode/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../postalcode/postalcode.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "postalcode", gfiles));
    }

}
