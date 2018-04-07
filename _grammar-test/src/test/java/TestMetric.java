import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestMetric {


    private static File gfile = new File("../metric/metric.g4");
    private static File [] ok = new File("../metric/examples").listFiles
            (pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "uom", gfile));
    }
}
