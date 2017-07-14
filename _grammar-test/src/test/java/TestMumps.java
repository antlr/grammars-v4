import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestMumps {

    private static File [] ok = new File("../mumps/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../mumps/mumps.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "program", gfile));
    }

}
