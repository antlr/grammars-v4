struct s {
    int i;
};

int main(void) {
    // declare variable with outer 'struct s' type
    struct s foo = {0};

    // declare another struct s type, shadowing the first
    // this is a distinct type even though its declaration is identical
    struct s {
        int i;
    };

    // invalid initializer: can't convert pointer to outer struct s (&foo)
    // to pointer to inner struct s (ptr)
    struct s *ptr = &foo;
}