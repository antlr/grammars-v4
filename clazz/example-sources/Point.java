public record Point(int x, int y) {
    public static Runnable printer() {
        return () -> System.out.println("point");
    }
}
