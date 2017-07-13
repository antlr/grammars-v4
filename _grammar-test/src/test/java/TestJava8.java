import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestJava8 {

    private static File [] ok = new File("../java8/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../java8/Java8.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "compilationUnit", gfile));
    }
}
