int main(void) {
    int *ptr;
    // this is malformed in two ways: the declarator itself is invalid,
    // and it's missing a type specifier
    int *array_pointer[3] = ([3](*)) ptr;
    return 0;
}