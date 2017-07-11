import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestVerilog {

    private static File [] ok = new File("../verilog/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../verilog/Verilog2001.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "source_text", gfiles));
    }

}
