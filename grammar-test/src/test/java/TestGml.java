import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestGml {

    private static File gfile = new File("../gml/gml.g4");
    private static File [] ok = new File("../gml/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "graph", gfile));
    }
}
