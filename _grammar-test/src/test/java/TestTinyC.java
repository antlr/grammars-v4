import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestTinyC {

    private static File [] ok = new File("../tinyc/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../tinyc/tinyc.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "program", gfiles));
    }

}
