/* Test that the algorithm runs until it converges;
 * some blocks need to be visited three times before the algorithm converges
 * */

int putchar(int c);

int target(int a, int b, int c, int d) {
    while (a > 0) {
        while (c > 0) {
            putchar(c + d);
            c = c - 1;
            if (d % 2) {
                c = c - 2;
            }
        }

        while (b > 0) {
            c = 10;  // this is not dead, b/c it's used in previous while
                     // loop, but it takes multiple passes for that
                     // information to propagate to this point
            b = b - 1;
        }

        a = a - 1;
    }
    return 0;
}

int main(void) {
    return target(5, 4, 3, 65);
}