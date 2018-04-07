import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestSnobol {

    private static File [] ok = new File("../snobol/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../snobol/snobol.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "prog", gfiles));
    }


}
