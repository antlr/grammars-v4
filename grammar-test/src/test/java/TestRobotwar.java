import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;


public class TestRobotwar {

    private static File [] ok = new File("../robotwars/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File gfile =  new File("../robotwars/robotwar.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfile));
    }

}
