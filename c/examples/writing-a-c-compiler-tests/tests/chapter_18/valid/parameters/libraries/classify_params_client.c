/* Test that we classify structure parameters correctly,
 * by passing a variety of structures as arguments.
 * Each test function takes only one argument.
 * */

#include "classify_params.h"

int main(void) {
    struct twelve_bytes s1 = {0, "lmnopqr"};
    if (!test_twelve_bytes(s1)) {
        return 1;
    }

    struct nested_ints s2 = {127, {2147483647, -128}};
    if (!test_nested_ints(s2)) {
        return 2;
    }

    struct flattened_ints s3 = {127, 2147483647, -128};
    if (!test_flattened_ints(s3)) {
        return 3;
    }

    struct large s4 = {200000, 23.25, "abcdefghi"};
    if (!test_large(s4)) {
        return 4;
    }

    struct two_ints s5 = {999, 888};
    if (!test_two_ints(s5)) {
        return 5;
    }

    struct nested_double s6 = {{25.125e3}};
    if (!test_nested_double(s6)) {
        return 6;
    }

    struct two_eightbytes s7 = {1000., 'x'};
    if (!test_two_eightbytes(s7)) {
        return 7;
    }

    struct pass_in_memory s8 = {1.7e308, -1.7e308, -2147483647, -9223372036854775807l};
    if (!test_pass_in_memory(s8)) {
        return 8;
    }

    return 0; // success
}