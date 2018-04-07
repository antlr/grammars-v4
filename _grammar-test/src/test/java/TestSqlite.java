import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestSqlite {

    private static File [] ok = new File("../sqlite/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../sqlite/SQLite.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "parse", gfiles));
    }

}
