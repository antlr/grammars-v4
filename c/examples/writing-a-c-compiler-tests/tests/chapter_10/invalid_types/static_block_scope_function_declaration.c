int main(void) {
    /* Can't have static storage class
     * on block-scope function declarations
     */
    static int foo(void);
    return foo();
}

static int foo(void) {
    return 0;
}