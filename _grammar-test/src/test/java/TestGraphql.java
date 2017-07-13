import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestGraphql {

    private static File gfile = new File("../graphql/GraphQL.g4");


    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }
}
