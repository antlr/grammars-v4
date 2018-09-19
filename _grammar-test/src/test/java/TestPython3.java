import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestPython3 {

    private static File [] ok = new File("../python3/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../python3/Python3.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "file_input", gfiles));
    }
}
