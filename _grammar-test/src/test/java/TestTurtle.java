import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestTurtle {

    private static File [] ok = new File("../turtle/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../turtle/TURTLE.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "turtleDoc", gfiles));
    }


}
