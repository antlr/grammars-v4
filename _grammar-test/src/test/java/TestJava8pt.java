import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestJava8pt {

    private static File [] ok = new File("../java8-pt/examples").listFiles
            (pathname -> pathname.isFile());


    private static File [] gfiles = new File [] {
            new File("../java8-pt/JavaLexer.g4"),
            new File("../java8-pt/JavaParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "compilationUnit", gfiles));
    }
}
