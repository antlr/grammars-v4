import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestInformix {

    private static File [] ok = new File("../informix/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../informix/informix.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "compilation_unit", gfile));
    }
}
