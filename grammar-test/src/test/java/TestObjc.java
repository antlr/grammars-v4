import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

public class TestObjc {

    private static Set<String> blist = new HashSet<>();

    static {
        blist.add("AllInOne.m");
    }

    private static File [] ok = new File("../objc/examples").listFiles
            (pathname -> pathname.isFile() && !blist.contains(pathname
                    .getName()));

    private static File [] gfiles = new File [] {
            new File("../objc/ObjectiveCParser.g4"),
            new File("../objc/one-step-processing/ObjectiveCLexer.g4"),
            new File("../objc/one-step-processing/ObjectiveCPreprocessorParser" +
                    ".g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "translationUnit", gfiles));
    }
}
