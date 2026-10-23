/* Test that we rerun alias analysis with each pipeline iteration */

int putchar(int c);

int foo(int *ptr) {
    putchar(*ptr);
    return 0;
}

int target(void) {
    int x = 10;  // this is a dead store
    int y = 65;
    int *ptr = &y;
    if (0) {
        // on our first pass through the pipeline it will look like x is
        // aliased; on later passes, after unreachable code elimination removes
        // this branch, we'll recognize that x is not aliased
        ptr = &x;
    }
    x = 5;     // this is a dead store, but we'll only recognize this after
               // rerunning alias analysis
    foo(ptr);  // we'll think this makes x live until we recognize that x is not
               // aliased

    return 0;
}

int main(void) {
    return target();
}