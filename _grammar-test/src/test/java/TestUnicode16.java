import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestUnicode16 {


    private static File gfile =  new File("../unicode/unicode16/classify.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }

}
