import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestAtl {

    private static File gfile = new File("../atl/ATL.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }
}
