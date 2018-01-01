import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestJava {

    private static File [] ok = new File("../java/examples").listFiles(pathname -> pathname.isFile());

    private static File[] gfiles = new File[]{
            new File("../java/JavaLexer.g4"),
            new File("../java/JavaParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "compilationUnit", gfiles));
    }
}
