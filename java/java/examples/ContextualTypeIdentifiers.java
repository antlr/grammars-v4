class when {}

class ContextualTypeIdentifiers {
    when field;
    when[] array;
    java.util.List<when> list;

    when identity(when value) {
        return value;
    }

    int sealed;

    void sealed() {}
}
