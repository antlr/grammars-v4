import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestP {

    private static File [] ok = new File("../p/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../p/p.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "prog", gfiles));
    }

}
