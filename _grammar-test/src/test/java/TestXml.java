import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestXml {

    private static File [] ok = new File("../xml/examples").listFiles(pathname -> pathname.isFile());


    private static File [] gfiles = new File [] {
            new File("../xml/XMLLexer.g4"),
            new File("../xml/XMLParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "document", gfiles));
    }
}
