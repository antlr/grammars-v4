import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestSharc {


    private static File [] gfiles = new File [] {
            new File("../sharc/SHARCLexer.g4"),
            new File("../sharc/SHARCParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfiles));
    }

}
