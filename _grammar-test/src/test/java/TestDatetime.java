import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestDatetime {

    private static File gfile = new File("../datetime/datetime.g4");
    private static File [] ok = new File("../datetime/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "date_time", gfile));
    }
}
