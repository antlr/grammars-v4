import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestObjc {

    private static File [] ok = new File("../objc/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File [] gfiles = new File [] {
            new File("../objc/ObjectiveCParser.g4"),
            new File("../objc/one-step-processing/ObjectiveCLexer.g4"),
            new File("../objc/one-step-processing/ObjectiveCPreprocessorParser" +
                    ".g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfiles));
    }

}
