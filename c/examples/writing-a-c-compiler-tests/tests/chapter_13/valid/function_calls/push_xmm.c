/* Test that we rewrite push instructions involving XMM registers.
 * NOTE: this test program won't exercise that rewrite rule until
 * you implement register allocation in Part III.
 */

int callee(double a, double b, double c, double d, double e, double f, double g,
           double h, double i, double j, double k) {
    if (a != 0.) {
        return 1;
    }
    if (b != 1.) {
        return 2;
    }
    if (c != 2.) {
        return 3;
    }
    if (d != 3.) {
        return 4;
    }
    if (e != 4.) {
        return 5;
    }
    if (f != 5.) {
        return 6;
    }
    if (g != 6.) {
        return 7;
    }
    if (h != 7.) {
        return 8;
    }
    if (i != 8.) {
        return 9;
    }
    if (j != 9.) {
        return 10;
    }
    if (k != 10.) {
        return 11;
    }

    return 0;  // success
}

int target(int a, int b, int c, int d, int e) {
    return callee(0., 1., 2., 3., 4., 5., e + 1., d + 3., c + 5., b + 7.,
                  a + 9.);
}

int main(void) {
    return target(1, 2, 3, 4, 5);
}
