/* Test initialization of nested structs with automatic storage duration,
 * including:
 * - partial initialization
 * - using mix of compound and single initializers to initialize nested structs
 * - arrays of structs, structs containing arrays
 * */

#include "nested_auto_struct_initializers.h"

int validate_full_initialization(struct outer *ptr) {
    if (ptr->one_l != -200l || ptr->two_struct.one_i != -171 ||
        ptr->two_struct.two_arr[0] != 200 ||
        ptr->two_struct.two_arr[1] != 202 ||
        ptr->two_struct.two_arr[2] != 203 || ptr->two_struct.three_u != 40u ||
        strcmp(ptr->three_msg, "Important message!") || ptr->four_d != -22. ||
        ptr->five_pair.a != 1 || ptr->five_pair.b != 2) {
        return 0;
    }

    return 1;  // success
}

int validate_partial_initialization(struct outer *ptr) {
    // validate explicitly initialized members
    if (ptr->one_l != 1000 || ptr->two_struct.one_i != 1 ||
        strcmp(ptr->three_msg, "Partial")) {
        return 0;
    }

    // validate that uninitialized members are 0
    if (ptr->two_struct.two_arr[0] || ptr->two_struct.two_arr[1] ||
        ptr->two_struct.two_arr[2] || ptr->two_struct.three_u || ptr->four_d ||
        ptr->five_pair.a || ptr->five_pair.b) {
        return 0;
    }

    return 1;  // success
}

int validate_mixed_initialization(struct outer *ptr) {
    // validate explicitly initialized elements
    if (ptr->one_l != 200 || ptr->two_struct.one_i != 20 ||
        ptr->two_struct.two_arr[0] != 21 || ptr->two_struct.three_u != 22u ||
        strcmp(ptr->three_msg, "mixed") || ptr->four_d != 10.0 ||
        ptr->five_pair.a != 99 || ptr->five_pair.b != 100) {
        return 0;
    }

    // validate elements that were not explicitly initialized in inner2
    if (ptr->two_struct.two_arr[1] || ptr->two_struct.two_arr[2]) {
        return 0;
    }

    return 1;  // success
}

int validate_array_of_structs(struct outer *struct_array) {
    // validate element 0
    if (struct_array[0].one_l != 1 || struct_array[0].two_struct.one_i != 2 ||
        struct_array[0].two_struct.two_arr[0] != 3 ||
        struct_array[0].two_struct.two_arr[1] != 4 ||
        struct_array[0].two_struct.two_arr[2] != 5 ||
        struct_array[0].two_struct.three_u != 6 ||
        strcmp(struct_array[0].three_msg, "7") ||
        struct_array[0].four_d != 8.0 || struct_array[0].five_pair.a != 9 ||
        struct_array[0].five_pair.b != 10) {
        return 0;
    }

    // validate element 1
    if (struct_array[1].one_l != 101 ||
        struct_array[1].two_struct.one_i != 102 ||
        struct_array[1].two_struct.two_arr[0] != 103 ||
        struct_array[1].two_struct.two_arr[1] != 104 ||
        struct_array[1].two_struct.two_arr[2] != 105 ||
        struct_array[1].two_struct.three_u != 106 ||
        strcmp(struct_array[1].three_msg, "107") ||
        struct_array[1].four_d != 108.0 || struct_array[1].five_pair.a != 109 ||
        struct_array[1].five_pair.b != 110) {
        return 0;
    }

    // validate element 2
    if (struct_array[2].one_l != 201 ||
        struct_array[2].two_struct.one_i != 202 ||
        struct_array[2].two_struct.two_arr[0] != 203 ||
        // remaining elements of two_struct should be 0 since they weren't
        // explicitly initialized
        struct_array[2].two_struct.two_arr[1] ||
        struct_array[2].two_struct.two_arr[2] ||
        struct_array[2].two_struct.three_u ||
        strcmp(struct_array[2].three_msg, "207") ||
        struct_array[2].four_d != 208.0 || struct_array[2].five_pair.a != 209 ||
        // five_pair.b should be 0 since it wasn't explicitly initialized
        struct_array[2].five_pair.b) {
        return 0;
    }

    // validate element 3: one_l is 301, everything else is 0
    if (struct_array[3].one_l != 301 || struct_array[3].two_struct.one_i ||
        struct_array[3].two_struct.two_arr[0] ||
        struct_array[3].two_struct.two_arr[1] ||
        struct_array[3].two_struct.two_arr[2] ||
        struct_array[3].two_struct.three_u || struct_array[3].three_msg ||
        struct_array[3].four_d || struct_array[3].five_pair.a ||
        struct_array[3].five_pair.b) {
        return 0;
    }

    return 1;  // success
}