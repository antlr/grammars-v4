import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestRcs {

    private static File gfile =  new File("../rcs/RCS.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }


}
