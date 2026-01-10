/* Make sure we don't clobber argument passed in EDX register by
 * performing a division operation that uses that register */

int x(int a, int b, int c, int d, int e, int f) {
    return a == 1 && b == 2 && c == 3 && d == 4 && e == 5 && f == 6;
}

int main(void) {
    int a = 4;
    return x(1, 2, 3, 4, 5, 24 / a);
}