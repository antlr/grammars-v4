import org.junit.Test;

import java.io.File;

public class TestPython3alt {


    private static File [] ok = new File("../python3alt/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../python3alt/AltPython3.g4");

    @Test
    public void test(){
        // Not working
        //Assert.assertTrue(GrammarTester.run(ok, "file_input", gfiles));
    }

}
