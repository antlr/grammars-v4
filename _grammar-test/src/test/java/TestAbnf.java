import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestAbnf {


    private static File gfile = new File("../abnf/Abnf.g4");
    private static File[] ok = {new File("../abnf/examples/iri.abnf"),
            new File("../abnf/examples/postal.abnf"),
            new File("../abnf/examples/rfc5322.abnf")};


    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "rulelist", gfile));
    }
}
