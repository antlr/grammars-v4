import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestXml {

    private static File [] ok = new File("../xml/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });


    private static File [] gfiles = new File [] {
            new File("../xml/XMLLexer.g4"),
            new File("../xml/XMLParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfiles));
    }
}
