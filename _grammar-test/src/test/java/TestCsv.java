import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestCsv {

    private static File gfile = new File("../csv/CSV.g4");
    private static File [] ok = new File("../csv/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "csvFile", gfile));
    }
}
