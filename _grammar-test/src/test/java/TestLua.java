import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestLua {

    private static File [] ok = new File("../lua/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../lua/Lua.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "chunk", gfile));
    }


}
