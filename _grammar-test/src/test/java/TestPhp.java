import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

public class TestPhp {

    private static Set<String> blist = new HashSet<String> ();

    static {
        blist.add("alternativeSyntax.php");
    }

    private static File [] ok = new File("../php/examples").listFiles(
            pathname -> pathname.isFile() && !blist.contains(pathname.getName())
    );

    private static File [] gfiles = new File [] {
            new File("../php/PHPLexer.g4"),
            new File("../php/PHPParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "htmlDocument", gfiles));
    }


}
