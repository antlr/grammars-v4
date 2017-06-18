import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.FileFilter;

public class TestMysql {

    private static File [] ok = new File("../mysql/examples").listFiles(new FileFilter() {
        @Override
        public boolean accept(File pathname) {
            return pathname.isFile();
        }
    });


    private static File [] gfiles = new File [] {
            new File("../mysql/MySQLLexer.g4"),
            new File("../mysql/MySQLParser.g4")
    };

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(gfiles));
    }


}
