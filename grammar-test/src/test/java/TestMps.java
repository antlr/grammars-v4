import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestMps {

    private static File [] ok = new File("../mps/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../mps/mps.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "modell", gfile));
    }


}
