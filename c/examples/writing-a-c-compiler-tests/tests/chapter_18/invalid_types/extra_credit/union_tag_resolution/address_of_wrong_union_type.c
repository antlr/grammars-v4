union u {
    int i;
    char c;
};

int main(void) {
    // declare variable with outer 'union u' type
    union u foo = {0};

    // declare another union u type, shadowing the first
    // this is a distinct type even though its declaration is identical
    union u {
        int i;
        char c;
    };

    // invalid initializer: can't convert pointer to outer union u (&foo)
    // to pointer to inner union u (ptr)
    union u *ptr = &foo;
}