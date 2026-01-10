/* Test that we never optimize away function calls,
 * even if they're dead stores (i.e. update dead variables)
 * because they can have side effects */
#if defined SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif

int putchar(int c);

int main(void) {
    // Make sure we don't optimize away this function call.
    // It would be safe to keep the function call, but optimize out
    // the store to x (i.e. get rid of movl %eax, %x), but our implementation
    // doesn't.
    int x = putchar(67);
    return 0;
}