public class Box<T extends Number> {
    private final T value;

    public Box(T value) {
        this.value = value;
    }

    public T get() {
        return value;
    }
}
