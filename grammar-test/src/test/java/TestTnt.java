import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestTnt {

    private static File [] ok = new File("../tnt/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../tnt/tnt.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "equation", gfiles));
    }

}
