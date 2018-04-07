import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestFortran {

    private static File gfile = new File("../fortran77/fortran77.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }
}
