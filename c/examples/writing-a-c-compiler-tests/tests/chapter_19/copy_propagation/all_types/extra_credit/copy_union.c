/* Propagate copies of whole unions */

union u {
    long l;
    int i;
};

int callee(union u a, union u b) {
    if (a.l != -100) {
        return 1; // fail
    }
    if (b.l != -100) {
        return 2; // fail
    }

    return 0; // success
}

int target(void) {
    union u u1 = {0};
    union u u2 = {-100};
    u1 = u2; // generates u1 = u2

    // Make sure we pass the same value for both arguments.
    // We don't need to worry that register coalescing
    // will interfere with this test, because unions
    // won't be stored in registers
    return callee(u1, u2);
}

int main(void) {
    return target();
}