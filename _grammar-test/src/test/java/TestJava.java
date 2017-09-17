import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestJava {

    private static File [] ok = new File("../java/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../java/Java.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "compilationUnit", gfile));
    }
}
