import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestSmtLibV2 {

    private static File [] ok = new File("../smtlibv2/examples").listFiles
            (pathname
            -> pathname.isFile());

    private static File gfile =  new File("../smtlibv2/SMTLIBv2.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "start", gfile));
    }
}
