/* Test initialization of nested static structs, including:
 * - partial initialization
 * - arrays of structs, structs containing arrays
 * - implicit conversion of scalar elements, array decay of string literals
 */

#include "nested_static_struct_initializers.h"

// structs defined in client but visible here
// validation functions defined here

// case 1: struct with no explicit initializer should be all zeros
// struct outer all_zeros;
int test_uninitialized(void) {
    // validate elements in struct outer
    if (all_zeros.one_l || all_zeros.three_msg || all_zeros.four_d) {
        return 0;
    }

    // validate elements in struct inner
    if (all_zeros.two_struct.one_i || all_zeros.two_struct.two_arr[0] ||
        all_zeros.two_struct.two_arr[1] || all_zeros.two_struct.two_arr[2] ||
        all_zeros.two_struct.three_u) {
        return 0;
    }

    return 1;  // success
}

// case 2: partially initialized struct
/*
    struct outer partial = {
        100l,
        {10, {10}},  // leave arr[1], arr[2], and y uninitialized
        "Hello!"};   // leave d uninitialized
*/
int test_partially_initialized(void) {
    // validate elements in struct outer
    if (partial.one_l != 100l || strcmp(partial.three_msg, "Hello!")) {
        return 0;
    }

    if (partial.four_d) {  // this wasn't explicitly initialized, should be 0
        return 0;
    }

    // validate elements in struct inner
    if (partial.two_struct.one_i != 10 || partial.two_struct.two_arr[0] != 10) {
        return 0;
    }

    if (partial.two_struct.two_arr[1] || partial.two_struct.two_arr[2] ||
        partial.two_struct
            .three_u) {  // not explicitly initialized, should be 0
        return 0;
    }

    return 1;  // success
}

// case 3: fully initialized struct
/*
    struct outer full = {
        18014398509481979l,
        {1000, "ok",
        4292870144u},  // can initialized signed char array w/ static string
        "Another message",
        2e12};
*/
int test_fully_intialized(void) {
    // validate elements in struct outer
    if (full.one_l != 18014398509481979l ||
        strcmp(full.three_msg, "Another message") || full.four_d != 2e12) {
        return 0;
    }

    // validate elemetns in string inner
    if (full.two_struct.one_i != 1000 || full.two_struct.two_arr[0] != 'o' ||
        full.two_struct.two_arr[1] != 'k' || full.two_struct.two_arr[2] != 0 ||
        full.two_struct.three_u != 4292870144u) {
        return 0;
    }

    return 1;  // success
}

// case 4: implicit conversion of scalar elements
/*
    struct outer converted = {
        10.5,  // 10l
        {
            2147483650u,  // -2147483646
            {
                15.6,             // 15
                17592186044419l,  // 3
                2147483777u       // -127
            },
            1152921506754330624ul  // 2147483648u
        },
        0ul,                   // null pointer
        9223372036854776833ul  // 9223372036854777856.0
    };
*/
int test_implicit_conversions(void) {
    // validate elements in struct outer
    if (converted.one_l != 10l || converted.three_msg != 0 ||
        converted.four_d != 9223372036854777856.0) {
        return 0;
    }

    // validate elements in struct inner
    if (converted.two_struct.one_i != -2147483646 ||
        converted.two_struct.two_arr[0] != 15 ||
        converted.two_struct.two_arr[1] != 3 ||
        converted.two_struct.two_arr[2] != -127 ||
        converted.two_struct.three_u != 2147483648u) {
        return 0;
    }

    return 1;  // success
}

// case 5: array of structures
/*
    struct outer struct_array[3] = {{1, {2, "ab", 3}, 0, 5},
                                        {6, {7, "cd", 8}, "Message", 9}};
*/
int test_array_of_structs(void) {
    // leave last element uninitialized

    // validate outer members of array element 0
    if (struct_array[0].one_l != 1 || struct_array[0].three_msg != 0 ||
        struct_array[0].four_d != 5) {
        return 0;
    }

    // validate nested members of array element 0
    if (struct_array[0].two_struct.one_i != 2 ||
        strcmp((char *)struct_array[0].two_struct.two_arr, "ab") ||
        struct_array[0].two_struct.three_u != 3) {
        return 0;
    }

    // validate outer members of array element 1
    if (struct_array[1].one_l != 6 ||
        strcmp((char *)struct_array[1].three_msg, "Message") ||
        struct_array[1].four_d != 9) {
        return 0;
    }

    // validate nested members of array element 1
    if (struct_array[1].two_struct.one_i != 7 ||
        strcmp((char *)struct_array[1].two_struct.two_arr, "cd") ||
        struct_array[1].two_struct.three_u != 8) {
        return 0;
    }

    // validate array element 2 - should be all 0s
    if (struct_array[2].one_l || struct_array[2].three_msg ||
        struct_array[2].four_d) {
        return 0;
    }

    // validate nested members of array element 2
    if (struct_array[2].two_struct.one_i ||
        struct_array[2].two_struct.two_arr[0] ||
        struct_array[2].two_struct.two_arr[1] ||
        struct_array[2].two_struct.two_arr[2] ||
        struct_array[2].two_struct.three_u) {
        return 0;
    }

    return 1;  // success
}
