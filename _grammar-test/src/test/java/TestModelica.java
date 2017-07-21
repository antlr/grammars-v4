import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestModelica {

    private static File [] ok = new File("../modelica/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../modelica/modelica.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "stored_definition", gfile));
    }

}
