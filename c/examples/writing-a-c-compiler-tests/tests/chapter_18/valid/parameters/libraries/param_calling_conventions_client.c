/* Test that we can pass a mix of struct and non-struct arguments according to
 * the ABI */

#include "param_calling_conventions.h"

int main(void) {
    // define a bunch of structures
    struct two_longs two_longs = {1234567l, 89101112l};
    struct one_int one_int = {54320, 'c'};
    struct one_int_exactly one_long = {567890l};
    struct two_ints two_ints = {'_', {5, 6, 7}};
    struct two_ints_nested two_ints_nested = {one_int, one_int};
    struct twelve_bytes xii = {123, "string!"};

    struct one_xmm one_xmm = {5.125};
    struct two_xmm two_xmm = {{55.5, 44.4}};
    struct int_and_xmm int_and_xmm = {'p', 4.56};
    struct xmm_and_int xmm_and_int = {{1.234}, "hi"};

    struct odd_size odd = {"lmno"};
    struct memory mem = {15.75, "rs", 4444, 3333};

    // call validation functions

    if (!pass_small_structs(two_xmm, one_int, one_xmm, xmm_and_int, xii,
                            one_long)) {
        return 1;
    }

    // based on example in Listing 18-45
    if (!a_bunch_of_arguments(0, 1, 2, 3, 4, two_longs, 5)) {
        return 2;
    }

    if (!structs_and_scalars(10, 10.0, odd, mem, one_xmm)) {
        return 2;
    }

    if (!struct_in_mem(10.0, 11.125, 12.0, xmm_and_int, 13.0, two_xmm, 0,
                       int_and_xmm, one_xmm)) {
        return 3;
    }
    if (!pass_borderline_struct_in_memory(two_ints, '!', int_and_xmm, 0,
                                          two_ints_nested, 7.8)) {
        return 4;
    }

    // define some more structs to use in last two test cases
    struct twelve_bytes struct1 = {-1, {127, 126, 125}};
    struct twelve_bytes struct2 = {-5, {100, 101, 102}};
    struct odd_size os = {{100, 99, 98, 97, 96}};
    struct memory m = {5.345, {-1, -2, -3}, 4294967300l, 10000};
    if (!pass_uneven_struct_in_mem(struct1, 9223372036854775805l,
                                   9223372036854775800l, struct2, os, m)) {
        return 5;
    }

    if (!pass_later_structs_in_regs(m, struct1, one_xmm)) {
        return 6;
    }

    // success!
    return 0;
}