import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestModula2Pim {

    private static File [] ok = new File("../modula2pim4/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../modula2pim4/m2pim4.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "compilationUnit", gfile));
    }

}
