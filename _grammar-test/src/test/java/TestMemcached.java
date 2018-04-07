import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestMemcached {

    private static File [] ok = new File("../memcached_protocol/examples")
            .listFiles(pathname -> pathname.isFile());

    private static File gfile =  new File
            ("../memcached_protocol/memcached_protocol.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "command_line", gfile));
    }

}
