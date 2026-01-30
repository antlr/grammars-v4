/* Test that we initialize each block in the CFG with an empty set of
 * live variables. Specifically, this test will fail if each block is
 * initialized with the set of all static variables.
 */

#if defined SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif

int j = 3;
int target(void) {
    static int i;
    i = 10;  // dead store, b/c i is killed on path to exit
             // but if we initially think i is live at the start of the
             // while loop, our analysis will never figure out that it's dead
             // here
    while (j > 0) {
        j = j - 1;
    }
    i = 0;
    return 0;
}

int main(void) {
    target();
    return (j == 0);  // just make sure target() actually did something
}