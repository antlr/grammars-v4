int main(void) {
    /* It's illegal to declare an identifier with external linkage and
     * no linkage in the same scope. Here, the function declaration foo
     * has external linkage and the variable declaration has no linkage.
     * The types here also conflict, but our implementation will catch
     * the linkage error before this gets to the type checker.
     */
    int foo(void);
    int foo = 1;
    return foo;
}

int foo(void) {
    return 1;
}