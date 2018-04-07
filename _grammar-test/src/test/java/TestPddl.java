import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestPddl {


    private static File gfile =  new File("../pddl/Pddl.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }


}
