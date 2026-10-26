#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif
int test_sum(long a, long b, int c, int d, int e, int f, int g, int h, long i) {
    /* Make sure the arguments passed in main weren't converted to ints */
    if (a + b < 100l) {
        return 1;
    }
    /* Check an argument that was passed on the stack too */
    if (i < 100l)
        return 2;
    return 0;
}

int main(void) {
    // passing a constant larger than INT_MAX as our last argument
    // exercises the rewrite rule for pushq $large_constant
    return test_sum(34359738368l, 34359738368l, 0, 0, 0, 0, 0, 0, 34359738368l);
}