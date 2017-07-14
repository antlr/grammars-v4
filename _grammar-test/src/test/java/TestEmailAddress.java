import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestEmailAddress {

    private static File gfile = new File("../emailaddress/emailaddress.g4");
    private static File [] ok = new File("../emailaddress/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "emailaddress", gfile));
    }
}
