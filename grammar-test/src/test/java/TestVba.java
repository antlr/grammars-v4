import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestVba {

    private static File [] ok = new File("../vba/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../vba/vba.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "startRule", gfiles));
    }

}
