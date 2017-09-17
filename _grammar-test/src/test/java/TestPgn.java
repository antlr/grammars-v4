import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestPgn {

    private static File [] ok = new File("../pgn/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../pgn/PGN.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "parse", gfiles));
    }

}
