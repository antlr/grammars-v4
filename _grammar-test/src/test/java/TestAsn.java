import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestAsn {

    private static File gfile = new File("../asn/ASN.g4");
    private static File [] ok = new File("../asn/examples").listFiles();

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "moduleDefinition", gfile));
    }
}
