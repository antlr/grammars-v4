struct inner {
    int i;
};

struct outer {
    struct inner foo;
};

int main(void) {
    struct outer x = {{1, 2}}; // sub-initializer for nested 'struct inner' has too many elements
    return 0;
}