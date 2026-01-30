/* This is essentially the same as the test case in tests/chapter11/valid/long_expressions/long_args.c,
 * but split across multiple files.
 */

int test_sum(int a, int b, int c, long d, int e, long f, int g, int h, long i);

int main(void) {
    return test_sum(0, 0, 0, 34359738368l, 0, 34359738368l, 0, 0, 34359738368l);
}