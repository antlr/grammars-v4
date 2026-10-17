int main(void) {
    for (int i = 0; i < 10; i = i + 1) {
        // test that struct tags are only in scope in block where they're
        // declared
        struct s {
            int a;
        };
    }

    // struct s tag is out of scope here
    // in our implementation this is illegal b/c struct s type is not in scope
    // in a fully conforming implementation this would be illegal b/c
    // it declares a variable with incomplete type
    struct s x;
    return 0;
}