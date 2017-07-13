import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestPeopleCode {

    private static File [] ok = new File("../peoplecode/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../peoplecode/PeopleCode.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "program", gfiles));
    }


}
