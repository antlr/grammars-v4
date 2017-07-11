import org.junit.Assert;
import org.junit.Test;

import java.io.File;


public class TestRobotwar {

    private static File [] ok = new File("../robotwars/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../robotwars/robotwar.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "program", gfiles));
    }

}
