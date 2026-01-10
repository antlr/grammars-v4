/* Verify that sizeof produces correct results for various expressions of
 * structure type. This is almost identical of sizeof_type except we're applying
 * sizeof to expressions and not just type specifiers.
 * This also tests that we correctly infer the types of expressions w/ structure
 * type
 * */

#include "struct_sizes.h"

struct twenty_bytes *get_twentybyte_ptr(void) {
    return 0;
}

int main(void) {
    // validate the size of every type in struct_sizes.h

    struct contains_struct_array arr_struct;

    if (sizeof arr_struct.struct_array[2] !=
        8) {  // elements of struct_array have type struct eight_bytes
        return 1;
    }

    static struct twentyfour_bytes twentyfour;
    if (sizeof twentyfour.seven.two2 != 2) {
        return 2;
    }

    if (sizeof get_twentybyte_ptr()->sixteen.three != 3) {
        return 3;
    }

    if (sizeof get_twentybyte_ptr()->sixteen != 16) {
        return 4;
    }

    if (sizeof twentyfour.seven != 7) {
        return 5;
    }

    if (sizeof twentyfour != 24) {
        return 6;
    }

    if (sizeof *get_twentybyte_ptr() != 20) {
        return 7;
    }

    if (sizeof *((struct wonky *)0) != 19) {
        return 8;
    }

    extern struct internal_padding struct_array[4];
    if (sizeof struct_array[0] != 16) {
        return 9;
    }

    if (sizeof arr_struct != 28) {
        return 10;
    }

    if (sizeof struct_array != 64) {
        return 11;
    }

    // make sure arr_struct.struct_array doesn't undergo array decay here
    if (sizeof arr_struct.struct_array != 24) {
        return 12;
    }

    return 0;  // success
}