/* Test that compound assignment expressions yield the correct value, have
 * the same precedence, and are right-associative.
 */
int main(void) {
    int a = 250;
    int b = 200;
    int c = 100;
    int d = 75;
    int e = -25;
    int f = 0;
    int x = 0;
    x = a += b -= c *= d /= e %= f = -7;
    return a == 2250 && b == 2000 && c == -1800 && d == -18 && e == -4 &&
           f == -7 && x == 2250;
}