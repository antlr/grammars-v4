import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestStacktrace {


    private static File gfile =  new File("../stacktrace/StackTrace.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }


}
