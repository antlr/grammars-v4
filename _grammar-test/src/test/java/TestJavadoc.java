import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestJavadoc {

    private static File [] ok = new File("../javadoc/examples").listFiles(pathname -> pathname.isFile());

    private static File [] gfiles = new File [] {
            new File("../javadoc/JavadocParser.g4"),
            new File("../javadoc/JavadocLexer.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "documentation", gfiles));
    }
}
