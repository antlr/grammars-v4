import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestGraphemes {

    private static File [] ok = new File("../unicode/graphemes/examples")
            .listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../unicode/graphemes/Graphemes.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "graphemes", gfile));
    }

}
