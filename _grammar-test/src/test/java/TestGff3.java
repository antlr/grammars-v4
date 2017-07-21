import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestGff3 {

    private static File gfile = new File("../gff3/gff3.g4");
    private static File [] ok = new File("../gff3/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "document", gfile));
    }
}
