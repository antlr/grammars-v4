/* Make sure that calling another function doesn't clobber
 * arguments to the current function passed in the same registers
 */

int g(int w, int x, int y, int z) {
    if (w == 2 && x == 4 && y == 6 && z == 8)
        return 1;
    return 0;
}

int f(int a, int b, int c, int d) {
    int result = g(a * 2, b * 2, c * 2, d * 2);
    return (result == 1 && a == 1 && b == 2 && c == 3 && d == 4);

}

int main(void) {
    return f(1, 2, 3, 4);
}