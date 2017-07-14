import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestClojure {

    private static File gfile = new File("../clojure/Clojure.g4");


    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }
}
