import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestCalculator {

    private static File gfile = new File("../calculator/calculator.g4");
    private static File [] ok = new File("../calculator/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "equation", gfile));
    }
}
