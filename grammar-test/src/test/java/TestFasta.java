import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestFasta {

    private static File gfile = new File("../fasta/fasta.g4");
    private static File [] ok = new File("../fasta/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "sequence", gfile));
    }
}
