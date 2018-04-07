import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestLolcode {

    private static File [] ok = new File("../lolcode/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../lolcode/lolcode.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "program", gfile));
    }

}
