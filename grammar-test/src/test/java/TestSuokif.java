import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestSuokif {

    private static File [] ok = new File("../suokif/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../suokif/SUOKIF.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "top_level", gfiles));
    }


}
