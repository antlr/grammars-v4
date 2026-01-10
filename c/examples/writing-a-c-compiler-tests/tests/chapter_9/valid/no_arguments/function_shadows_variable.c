int main(void) {
    int foo = 3;
    int bar = 4;
    if (foo + bar > 0) {
        /* Function declaration foo shadows variable foo */
        int foo(void);
        bar = foo();
    }
    /* Variable foo becomes visible again */
    return foo + bar;
}

int foo(void) {
    return 8;
}