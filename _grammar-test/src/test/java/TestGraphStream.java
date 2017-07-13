import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestGraphStream {

    private static File [] ok = new File("../graphstream-dgs/examples")
            .listFiles();

    private static File [] gfiles =  new File [] {
            new File("../graphstream-dgs/DGSLexer.g4"),
            new File("../graphstream-dgs/DGSParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "dgs", gfiles));
    }
}
