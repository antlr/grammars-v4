import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestPropcalc {

    private static File [] ok = new File("../propcalc/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../propcalc/propcalc.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "proposition", gfiles));
    }

}
