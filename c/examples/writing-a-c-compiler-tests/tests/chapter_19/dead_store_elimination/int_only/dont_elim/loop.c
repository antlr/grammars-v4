/* Test case where a block is its own predecessor
 * */

int putchar(int c);

int fib(int count) {
    int n0 = 0;
    int n1 = 1;
    int i = 0;
    do {
        int n2 = n0 + n1;
        n0 = n1;  // not a dead store b/c n0 is used again in the next loop
                  // iteration, in n2 = n0 + n1
        n1 = n2;
        i = i + 1;
    } while (i < count);
    return n1;
}

int main(void) {
    return (fib(20) == 10946);
}