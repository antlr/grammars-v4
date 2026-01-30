/* Test initialization of non-nested static structs, including:
 * - partial initialization
 * - implicit conversion of scalar elements
 * - array decay of string literals
 */

#include "static_struct_initializers.h"

// structs defined in client but visible here
// validation functions defined here

// case 1: struct with no explicit initializer should be all zeros
// struct s uninitialized;
int test_uninitialized(void) {
    // make sure all elements are zero
    if (uninitialized.one_d || uninitialized.two_msg ||
        uninitialized.three_arr[0] || uninitialized.three_arr[1] ||
        uninitialized.three_arr[2] || uninitialized.four_i) {
        return 0;
    }
    return 1;  // success
}

// case 2: partially initialized struct
// struct s partial = {1.0, "Hello"};
int test_partially_initialized(void) {
    // validate first two elements
    if (partial.one_d != 1.0 || strcmp(partial.two_msg, "Hello")) {
        return 0;
    }

    // validate that remaining elements are zero
    if (partial.three_arr[0] || partial.three_arr[1] || partial.three_arr[2] ||
        partial.four_i) {
        return 0;
    }

    return 1;  // success
}

// case 3: partially initialized array w/in struct
// struct s partial with_array = {3.0, "!", {1}, 2};
int test_partial_inner_init(void) {
    // validate explicitly initialzed elements
    if (partial_with_array.one_d != 3.0 ||
        strcmp(partial_with_array.two_msg, "!") ||
        partial_with_array.three_arr[0] != 1 ||
        partial_with_array.four_i != 2) {
        return 0;
    }

    // validate that last two elements of arr are 0
    if (partial_with_array.three_arr[1] || partial_with_array.three_arr[2]) {
        return 0;
    }

    return 1;  // success
}

// case 4: implicit conversion of scalar elements
/*
    struct s converted = {
        1152921504606846977l,  // 1152921504606846976.0
        0l,                   // null ptr
        "abc",                // {'a', 'b', 'c'}
        17179869189l          // 5
    };
*/
int test_implicit_conversion(void) {
    // validate elements
    if (converted.one_d != 1152921504606846976.0 || converted.two_msg ||
        converted.three_arr[0] != 'a' || converted.three_arr[1] != 'b' ||
        converted.three_arr[2] != 'c' || converted.four_i != 5) {
        return 0;
    }

    return 1;  // success
}