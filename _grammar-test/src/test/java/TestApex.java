import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestApex {

    private static File gfile = new File("../apex/apex.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }
}
