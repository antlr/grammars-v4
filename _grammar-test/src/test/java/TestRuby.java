import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestRuby {

    private static File [] ok = new File("../ruby/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../ruby/Corundum.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "prog", gfiles));
    }


}
