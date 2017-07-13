import org.junit.Assert;
import org.junit.Test;

import java.io.File;

public class TestIcalendar {

    private static File [] ok = new File("../icalendar/examples").listFiles(pathname -> pathname.isFile());
    private static File gfile =  new File("../icalendar/ICalendar.g4");

    @Test
    public void test(){
        Assert.assertTrue(GrammarTester.run(ok, "parse", gfile));
    }
}
