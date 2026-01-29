/* Test that we can optimize away a for loop that will never execute;
 * initial expression still runs but post expression and body don't.
 * */
#if defined SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wdiv-by-zero"
#endif

int callee(void) {
    return 1 / 0;
}

int target(void) {
    int i = 0;
    for (i = 10; 0; i = callee()) callee();
    return i;
}

int main(void) {
    return target();
}