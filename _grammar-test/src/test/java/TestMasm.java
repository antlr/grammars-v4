import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestMasm {

    private static File gfile =  new File("../masm/MASM.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }


}
