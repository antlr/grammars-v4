/* Test that we eliminate y = x and y = x if we can prove that x and y
 * already have the same values.
 * After copy propagation and cleanup unreachable code elimination,
 * target should contain no control-flow instructions.
 * Similar to int_only/redundant_copies.c but with structs
 * */

struct s {
    double d;
    int i;
};

double target(int flag, int flag2, struct s y) {
    struct s x = y;

    if (flag) {
        y = x;  // we can remove this because x and y already have the same
                // value
    }
    if (flag2) {
        x = y;  // we can remove this because x and y already have the same
                // value
    }
    return x.d + y.d + x.i + y.i;
}

int main(void) {
    struct s my_struct = {25.0, 5};
    return target(0, 1, my_struct) == 60.0;
}