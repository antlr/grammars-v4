/* Test that we can propagate copies of aggregate values */
struct s {
    int x;
    int y;
};

int callee(struct s a, struct s b) {
    if (a.x != 3) {
        return 1; // fail
    }
    if (a.y != 4) {
        return 2; // fail
    }
    if (b.x != 3) {
        return 3; // fail
    }
    if (b.y != 4) {
        return 4; // fail
    }
    return 0; // success
}

int target(void) {
    struct s s1 = {1, 2};
    struct s s2 = {3, 4};
    s1 = s2;  // generate s1 = s2

    // Make sure we pass the same value for both arguments.
    // We don't need to worry that register coalescing
    // will interfere with this test,
    // because s1 and s2, as structures, won't be stored in registers.
    return callee(s1, s2);
}

int main(void) {
    return target();
}