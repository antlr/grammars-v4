/* Test that we eliminate y = x and y = x if we can prove that x and y
 * already have the same values.
 * After copy propagation and cleanup unreachable code elimination,
 * target should contain no control-flow instructions.
 * Similar to int_only/redundant_copies.c but with doubles
 * */

double target(int flag, int flag2, double y) {
    double x = y;

    if (flag) {
        y = x;  // we can remove this because x and y already have the same
                // value
    }
    if (flag2) {
        x = y;  // we can remove this because x and y already have the same
                // value
    }
    return x + y;
}

int main(void) {
    if (target(0, 1, 10.0) != 20.0) {
        return 1; // fail
    }
    return 0;
}