import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestVerilog {

    private static File [] ok = new File("../verilog/verilog/examples").listFiles(pathname -> pathname.isFile());

    private static File [] gfiles = new File [] {
        new File("../verilog/verilog/VerilogLexer.g4"),
        new File("../verilog/verilog/VerilogParser.g4"),
        new File("../verilog/verilog/VerilogPreprocessorParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "source_text", gfiles));
    }

}
