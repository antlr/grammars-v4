import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;


public class TestVb6 {

    static Set<String> blist = new HashSet<>();

    private static File [] ok = null;

    static {
        blist.add("form1.vb");
    }

    private static File gfiles =  new File("../vb6/VisualBasic6.g4");

    @Test
    public void test(){
        Set<File> ps = null;
        try {
            ps = Files.walk(Paths.get("../vb6/examples"))
                    .filter(Files::isRegularFile)
                    .filter(f -> !blist.contains(f.getFileName().toString()))
                    .filter(f -> !f.getFileName().endsWith(".tree"))
                    .map(f -> f.toFile()).collect(Collectors.toSet());
        } catch (IOException e) {
            e.printStackTrace();
        }

        ok = ps.toArray(new File[ps.size()]);

        Assert.assertTrue(GrammarTester.run(ok, "startRule", gfiles));
    }

}
