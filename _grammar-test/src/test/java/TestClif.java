import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestClif {

    private static File gfile = new File("../clif/CLIF.g4");


    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }
}
