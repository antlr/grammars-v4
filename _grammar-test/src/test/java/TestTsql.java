import org.junit.Assert;
import org.junit.Test;
import org.snt.inmemantlr.GenericParser;

import java.io.File;

public class TestTsql {

    private static File [] ok = new File("../tsql/examples").listFiles(pathname -> pathname.isFile());

    private static File[] gfiles = new File[]{
            new File("../tsql/TSqlLexer.g4"),
            new File("../tsql/TSqlParser.g4")
    };


    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(GenericParser.CaseSensitiveType
                .UPPER,ok, "tsql_file", gfiles));
    }

}
