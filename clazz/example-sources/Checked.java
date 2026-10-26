import java.io.IOException;

public class Checked {
    public static int parse(String value) throws IOException {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException exception) {
            throw new IOException("invalid number", exception);
        }
    }
}
