import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestXpath {

    private static File [] ok = new File("../xpath/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../xpath/xpath.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "main", gfiles));
    }

}
