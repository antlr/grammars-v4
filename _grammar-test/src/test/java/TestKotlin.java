import org.junit.Assert;
import org.junit.Test;

import java.io.File;


public class TestKotlin {

    private static File [] ok = new File("../kotlin/examples").listFiles(pathname -> pathname.isFile());


    private static File [] gfiles = new File [] {
            new File("../kotlin/KotlinParser.g4"),
            new File("../kotlin/KotlinLexer.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "kotlinFile", gfiles));
    }

}
