import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestProperties {

    private static File [] ok = new File("../properties/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../properties/properties.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "propertiesFile", gfiles));
    }


}
