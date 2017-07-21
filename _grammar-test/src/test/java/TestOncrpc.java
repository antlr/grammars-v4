import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestOncrpc {

    private static File [] ok = new File("../oncrpc/examples").listFiles(pathname -> pathname.isFile());

    private static File [] gfiles =  new File [] {
            new File("../oncrpc/xdr.g4"),
            new File("../oncrpc/oncrpcv2.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "oncrpcv2Specification", gfiles));
    }


}
