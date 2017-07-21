import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestWavefront {

    private static File [] ok = new File("../wavefront/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../wavefront/WavefrontOBJ.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "start", gfiles));
    }


}
