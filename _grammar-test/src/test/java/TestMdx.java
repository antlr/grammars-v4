import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestMdx {

    private static File [] ok = new File("../mdx/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../mdx/mdx.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "mdx_statement", gfile));
    }

}
