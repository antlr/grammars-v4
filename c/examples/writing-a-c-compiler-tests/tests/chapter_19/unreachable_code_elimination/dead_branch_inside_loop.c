/* Test that we can eliminate dead code inside of a larger, non-dead
 * control structure
 * */
#if defined SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wdiv-by-zero"
#endif

int callee(void) {
    return 1 / 0;
}

int target(void) {
    int result = 105;
    // loop is not optimized away but inner function call is
    for (int i = 0; i < 100; i = i + 1) {
        if (0) {  // this if statement and function call should be optimized
                  // away
            return callee();
        }
        result = result - i;
    }
    return result;
}

int main(void) {
    if (target() != -4845) {
        return 1; // fail
    }
    return 0; // success
}