import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestProtobuf {

    private static File [] ok = new File("../protobuf3/examples").listFiles(pathname -> pathname.isFile());

    private static File gfiles =  new File("../protobuf3/Protobuf3.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "proto", gfiles));
    }


}
