import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestErlang {

    private static File gfile = new File("../erlang/Erlang.g4");
    private static File [] ok = new File("../erlang/examples").listFiles(pathname -> pathname.isFile() && pathname.getName().endsWith("erl"));

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "forms", gfile));
    }
}
