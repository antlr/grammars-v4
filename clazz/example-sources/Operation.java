public interface Operation {
    int apply(int value);

    default int twice(int value) {
        return apply(apply(value));
    }
}
