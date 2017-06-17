import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestJavadoc {

    private static File [] ok = new File("../javadoc/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File [] gfiles = new File [] {
            new File("../javadoc/JavadocParser.g4"),
            new File("../javadoc/JavadocLexer.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfiles));
    }


}
