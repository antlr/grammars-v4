import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestFusionTables {

    private static File gfile = new File("../fusion-tables/FusionTablesSql.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfile));
    }
}
