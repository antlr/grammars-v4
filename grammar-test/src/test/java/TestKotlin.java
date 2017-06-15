import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;


public class TestKotlin {

    private static File [] ok = new File("../kotlin/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });


    private static File [] gfiles = new File [] {
            new File("../kotlin/KotlinParser.g4"),
            new File("../kotlin/KotlinLexer.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfiles));
    }

}
