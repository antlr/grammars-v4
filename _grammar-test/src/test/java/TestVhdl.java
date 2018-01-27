import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestVhdl {

    private static File [] ok = new File("../vhdl/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../vhdl/vhdl.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "design_file", gfiles));
    }


}
