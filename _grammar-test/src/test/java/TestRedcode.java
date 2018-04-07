import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestRedcode {

    private static File [] ok = new File("../redcode/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../redcode/redcode.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "file", gfiles));
    }

}
