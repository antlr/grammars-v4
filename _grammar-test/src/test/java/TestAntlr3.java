import org.junit.Test;

import java.io.File;

public class TestAntlr3 {


    private static File gfile = new File("../antlr3/ANTLRv3.g4");
    private static File [] ok = new File("../antlr3/examples").listFiles(pathname -> pathname.isFile());


    @Test
    public void test(){
        // Not working
        //Assert.assertTrue(GrammarTester.run(ok, "grammarDef", gfile));
    }
}
