import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestUrl {

    private static File [] ok = new File("../url/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../url/url.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "url", gfiles));
    }


}
