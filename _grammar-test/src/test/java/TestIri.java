import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestIri {

    private static File [] ok = new File("../iri/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../iri/IRI.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "parse", gfile));
    }
}
