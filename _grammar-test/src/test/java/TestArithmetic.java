import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestArithmetic {


    private static File gfile = new File("../arithmetic/arithmetic.g4");
    private static File [] ok = new File("../arithmetic/examples").listFiles();

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "equation", gfile));
    }
}
