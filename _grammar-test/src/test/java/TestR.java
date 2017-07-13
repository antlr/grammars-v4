import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestR {

    private static File gfile =  new File("../r/R.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }

}
