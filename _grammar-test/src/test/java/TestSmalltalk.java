import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestSmalltalk {

    private static File [] ok = new File("../smalltalk/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../smalltalk/Smalltalk.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "script", gfiles));
    }

}
