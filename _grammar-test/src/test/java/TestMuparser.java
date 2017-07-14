import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestMuparser {

    private static File [] ok = new File("../muparser/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../muparser/MuParser.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "prog", gfile));
    }

}
