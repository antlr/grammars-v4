#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wreturn-type"
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif
int foo(void) {
    /* It's legal for a non-void function to not return a value.
     * If the caller tries to use the value of the function, the result is undefined.
     */
    int x = 1;
}

int main(void) {
    /* This is well-defined because we call foo but don't use its return value */
    foo();
    return 3;
}