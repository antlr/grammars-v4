import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestPcre {

    private static File [] ok = new File("../pcre/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../pcre/PCRE.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "parse", gfiles));
    }
}
