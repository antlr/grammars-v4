import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestCobol {

    private static File gfile = new File("../cobol85/Cobol85.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }
}
