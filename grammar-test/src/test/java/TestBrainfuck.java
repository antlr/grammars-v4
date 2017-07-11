import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestBrainfuck {

    private static File gfile = new File("../brainfuck/brainfuck.g4");
    private static File [] ok = new File("../brainfuck/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "file", gfile));
    }
}
