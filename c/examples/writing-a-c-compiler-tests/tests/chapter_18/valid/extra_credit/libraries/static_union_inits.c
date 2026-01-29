// Test initialization of static unions; make sure uninitialized unions are initialized to zero
#include "static_union_inits.h"


int validate_simple(void) {
    return (s.c == -39 && s.i == 217);
}

int validate_has_union(void) {
    return (h.u.c == 77 && h.c == 77 && h.u.i == 77);
}

int validate_has_union_array(void) {

    // validate array of unions
    // first validate elements 0-2
    for (int i = 0; i < 3; i = i + 1) {
        int expected = 'a' + i;
        if (my_struct.union_array[i].u.c != expected
            || my_struct.union_array[i].c != expected
            || my_struct.union_array[i].u.i != expected) {
            return 0;
        }
    }

    // last array element should be all 0s (including bytes that
    // aren't part of first member) b/c it's uninitialized
    if (my_struct.union_array[3].u.d != 0.0) {
        return 0;
    }

    // validate other elements of struct
    if (my_struct.c != '#') {
        return 0; // fail
    }

    if (my_struct.s.c != '!' || my_struct.s.i != '!') {
        return 0; // fail
    }

    return 1;
}

int validate_uninitialized(void) {
    if (all_zeros.u.d != 0.0) {
        return 0; // fail
    }
    return 1;
}

int validate_padded_union_array(void) {
    if (strcmp(padded_union_array[0].arr, "first string") != 0) {
        return 0; // fail
    }

    if (strcmp(padded_union_array[1].arr, "string #2") != 0) {
        return 0; // fail
    }

    if (strcmp(padded_union_array[2].arr, "string #3") != 0) {
        return 0; // fail
    }

    return 1;
}