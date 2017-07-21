import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestUseragent {

    private static File [] ok = new File("../useragent/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../useragent/useragent.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "prog", gfiles));
    }


}
