import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestJpa {

    private static File [] ok = new File("../jpa/examples").listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File("../jpa/JPA.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "ql_statement", gfile));
    }

}
