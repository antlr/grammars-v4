import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestSexpression {

    private static File [] ok = new File("../sexpression/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../sexpression/sexpression.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "sexpr", gfiles));
    }

}
