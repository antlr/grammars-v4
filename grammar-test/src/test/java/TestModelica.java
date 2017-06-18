import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestModelica {

    private static File [] ok = new File("../modelica/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });

    private static File gfile =  new File("../modelica/modelica.g4");
    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok,gfile));
    }

}
