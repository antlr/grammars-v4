import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestAsm6502 {

    private static File gfile = new File("../asm6502/asm6502.g4");
    private static File [] ok = new File("../asm6502/examples").listFiles();

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "prog", gfile));
    }
}
