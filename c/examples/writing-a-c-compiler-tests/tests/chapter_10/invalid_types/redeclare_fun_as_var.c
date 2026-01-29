int foo(void) {
    return 0;
}

int main(void) {
    /* Since this declaration has external linkage,
     * it refers to the same entity as the declaration
     * of foo above. But the earlier declaration declares
     * a function and this one declares a variable,
     * so they conflict.
     */
    extern int foo;
    return 0;
}