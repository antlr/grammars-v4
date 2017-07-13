import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestLambda {

    private static File [] ok = new File("../lambda/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../lambda/lambda.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "expression", gfile));
    }


}
