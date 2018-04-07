import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestDcm {

    private static File gfile = new File("../dcm/DCM_2_0_grammar.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }
}
