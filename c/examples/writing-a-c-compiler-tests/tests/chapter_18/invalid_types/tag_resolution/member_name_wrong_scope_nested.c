struct s {
    int a;
};

int main(void) {
    struct outer {
        struct s inner;
    };

    struct outer foo = {{1}};

    // introduce a different struct s type
    struct s {
        int b;
    };

    struct outer *ptr = &foo;

    return ptr->inner.b;  // foo.inner belongs to first struct s type, which
                          // doesn't have member 'b'
}