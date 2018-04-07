import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestJson {

    private static File [] ok = new File("../json/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../json/JSON.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "json", gfile));
    }

}
