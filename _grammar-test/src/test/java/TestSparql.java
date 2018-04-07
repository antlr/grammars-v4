import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestSparql {

    private static File [] ok = new File("../sparql/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../sparql/Sparql.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "query", gfiles));
    }

}
