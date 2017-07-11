import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestPascal {

    private static File [] ok = new File("../pascal/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../pascal/pascal.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "program", gfiles));
    }

}
