struct inner {
    int i;
};

struct outer {
    struct inner foo;
};

struct outer x = {{1, 2}}; // sub-initializer for nested 'struct inner' has too many elements
