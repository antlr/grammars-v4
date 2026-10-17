int main(void) {
    // invalid declarator syntax: can't have parenthesized (*) in non-abstract declarator
    int (*)(ptr_to_array[3]) = 0;
    return 0;
}