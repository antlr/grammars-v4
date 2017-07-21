import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestScala {

    private static File [] ok = new File("../scala/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../scala/Scala.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "compilationUnit", gfiles));
    }

}
