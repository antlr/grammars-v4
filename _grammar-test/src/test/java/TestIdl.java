import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestIdl {

    private static File [] ok = new File("../idl/examples").listFiles(pathname -> pathname.isFile());
    private static File gfile =  new File("../idl/IDL.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "specification", gfile));
    }
}
