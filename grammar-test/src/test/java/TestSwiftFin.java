import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestSwiftFin {

    private static File [] ok = new File("../swift-fin/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File [] gfiles =  new File [] {
            new File("../swift-fin/src/main/antlr4/SwiftFinLexer.g4"),
            new File("../swift-fin/src/main/antlr4/SwiftFinParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfiles));
    }


}
