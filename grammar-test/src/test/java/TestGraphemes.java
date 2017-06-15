import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestGraphemes {


    private static File gfile =  new File("../unicode/graphemes/Graphemes.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }

}
