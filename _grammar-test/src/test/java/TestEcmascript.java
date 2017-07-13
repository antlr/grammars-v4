import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestEcmascript {

    private static File gfile = new File("../ecmascript/ECMAScript.g4");
    private static File [] ok = new File("../ecmascript/examples").listFiles(pathname -> pathname.isFile());

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "program", gfile));
    }
}
