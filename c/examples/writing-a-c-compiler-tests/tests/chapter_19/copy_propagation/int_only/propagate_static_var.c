/* Propagate a copy where the source value is a variable with static storage
 * duration, in a function with no control flow strucures.
 * */
int callee(int a, int b) {
    return a + b;
}

int target(void) {
    static int x = 3;

    // y also needs to be static so we can't coalesce
    // it into ESI once we implement register coalescing;
    // otherwise it may look like we've propagated a copy
    // when we haven't
    static int y = 0;

    y = x;  // make sure we propagate this into function call

    // look for: same value passed in ESI, EDI
    int sum = callee(x, y);

    // increment x to make sure we're not just propagating
    // x's initial value. (If we are, we'll get the wrong result on
    // the second call to target
    x = x + 1;
    return sum;
}

int main(void) {
    // make sure target gives correct result
    if (target() != 6) {
        return 1;
    }
    // invoke again after x has been incremented
    if (target() != 8) {
        return 2;
    }
    return 0;  // success
}
