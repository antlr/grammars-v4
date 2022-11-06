public class Switch {
    public static void main(String[] args)
    {
        String myString = "some_text";
        int j = switch (myString) {
            case "some_text"  -> 0;
            default      -> {
                int result = 10;
                yield result;
            }
        };
    }
}
