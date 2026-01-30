/* Test that we classify structure parameters correctly,
 * by passing a variety of structures as arguments.
 * Each test function takes only one argument.
 * */

#include "classify_params.h"

int test_twelve_bytes(struct twelve_bytes s) {
    if (s.i != 0 || strcmp(s.arr, "lmnopqr")) {
        return 0;
    }
    return 1;  // success
}
int test_nested_ints(struct nested_ints s) {
    if (s.ch1 != 127 || s.nested.i != 2147483647 || s.nested.ch2 != -128) {
        return 0;
    }
    return 1;  // success
}
int test_flattened_ints(struct flattened_ints s) {
    if (s.c != 127 || s.i != 2147483647 || s.a != -128) {
        return 0;
    }

    return 1;  // success
}
int test_large(struct large s) {
    if (s.i != 200000 || s.d != 23.25 || strcmp(s.arr, "abcdefghi")) {
        return 0;
    }

    return 1;  // success
}
int test_two_ints(struct two_ints s) {
    if (s.i != 999 || s.i2 != 888) {
        return 0;
    }

    return 1;  // success
}
int test_nested_double(struct nested_double s) {
    if (s.array[0] != 25.125e3) {
        return 0;
    }

    return 1;  // success
}
int test_two_eightbytes(struct two_eightbytes s) {
    if (s.d != 1000. || s.c != 'x') {
        return 0;
    }

    return 1;  // success
}
int test_pass_in_memory(struct pass_in_memory s) {
    if (s.w != 1.7e308 || s.x != -1.7e308 || s.y != -2147483647 ||
        s.z != -9223372036854775807l) {
        return 0;
    }

    return 1;  // success
}