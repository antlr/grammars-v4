/* Division requires us to use the RDX register;
 * make sure this doesn't clobber the argument passed
 * in this register
 */
int f(int a, int b, int c, int d) {
    // perform division
    int x = a / b;
    // make sure everything has the right value
    if (a == 10 && b == 2 && c == 100 && d == 4 && x == 5)
        return 1;
    return 0;
}